// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// Represents symbols imported to the binding scope via using namespace, using alias, and extern alias.
    /// </summary>
    internal abstract partial class Imports
    {
        internal static readonly Imports Empty = new Eager(
            compilation: null, 
            usingAliases: null,
            usings: ImmutableArray<NamespaceOrTypeAndUsingDirective>.Empty, 
            externs: ImmutableArray<AliasAndExternAliasDirective>.Empty, 
            diagnostics: ImmutableArray<Diagnostic>.Empty);

        private readonly CSharpCompilation _compilation;

        // completion state that tracks whether validation was done/not done/currently in process. 
        private SymbolCompletionState _state;

        public readonly ImmutableArray<AliasAndExternAliasDirective> ExternAliases;

        protected abstract Dictionary<string, AliasAndUsingDirective> GetUsingAliasesInternal(ConsList<Symbol> basesBeingResolved);
        protected abstract ImmutableArray<NamespaceOrTypeAndUsingDirective> GetUsingsInternal(ConsList<Symbol> basesBeingResolved);

        protected abstract ImmutableArray<Diagnostic> Diagnostics { get; }

        protected Imports(ImmutableArray<AliasAndExternAliasDirective> externs, CSharpCompilation compilation)
        {
            Debug.Assert(!externs.IsDefault);

            ExternAliases = externs;
            _compilation = compilation;
        }

        public static Imports FromSyntax(
            CSharpSyntaxNode declarationSyntax,
            InContainerBinder binder,
            ConsList<Symbol> basesBeingResolved)
        {
            SyntaxList<UsingDirectiveSyntax> usingDirectives;
            SyntaxList<ExternAliasDirectiveSyntax> externAliasDirectives;
            if (declarationSyntax.Kind() == SyntaxKind.CompilationUnit)
            {
                var compilationUnit = (CompilationUnitSyntax)declarationSyntax;
                // using directives are not in scope within using directives
                usingDirectives = compilationUnit.Usings;
                externAliasDirectives = compilationUnit.Externs;
            }
            else if (declarationSyntax.Kind() == SyntaxKind.NamespaceDeclaration)
            {
                var namespaceDecl = (NamespaceDeclarationSyntax)declarationSyntax;
                // using directives are not in scope within using directives
                usingDirectives = namespaceDecl.Usings;
                externAliasDirectives = namespaceDecl.Externs;
            }
            else
            {
                return Empty;
            }

            if (usingDirectives.Count == 0)
            {
                if (externAliasDirectives.Count == 0)
                {
                    return Empty;
                }

                DiagnosticBag diagnostics = DiagnosticBag.GetInstance();
                var externs = BuildExternAliases(externAliasDirectives, binder, diagnostics);
                return new Eager(
                    binder.Compilation, 
                    usingAliases: null, 
                    usings: ImmutableArray<NamespaceOrTypeAndUsingDirective>.Empty, 
                    externs: externs, 
                    diagnostics: diagnostics.ToReadOnlyAndFree());
            }

            // define all of the extern aliases first. They may used by the target of a using

            // using Bar=Foo::Bar;
            // using Foo::Baz;
            // extern alias Foo;

            DiagnosticBag externDiagnostics = DiagnosticBag.GetInstance();
            return new Lazy(
                binder,
                BuildExternAliases(externAliasDirectives, binder, externDiagnostics),
                usingDirectives,
                externDiagnostics.ToReadOnlyAndFree());
        }

        public static Imports FromGlobalUsings(CSharpCompilation compilation)
        {
            var usings = compilation.Options.Usings;
            var diagnostics = DiagnosticBag.GetInstance();
            var usingsBinder = new InContainerBinder(compilation.GlobalNamespace, new BuckStopsHereBinder(compilation)).WithAdditionalFlags(BinderFlags.IgnoreUsings);
            var boundUsings = ArrayBuilder<NamespaceOrTypeAndUsingDirective>.GetInstance();

            foreach (string ns in usings)
            {
                if (!ns.IsValidClrNamespaceName())
                {
                    continue;
                }

                string[] identifiers = ns.Split('.');
                NameSyntax qualifiedName = SyntaxFactory.IdentifierName(identifiers[0]);

                for (int j = 1; j < identifiers.Length; j++)
                {
                    qualifiedName = SyntaxFactory.QualifiedName(left: qualifiedName, right: SyntaxFactory.IdentifierName(identifiers[j]));
                }

                boundUsings.Add(new NamespaceOrTypeAndUsingDirective(usingsBinder.BindNamespaceOrTypeSymbol(qualifiedName, diagnostics), null));
            }

            return new Eager(compilation, null, boundUsings.ToImmutableAndFree(), ImmutableArray<AliasAndExternAliasDirective>.Empty, diagnostics.ToReadOnlyAndFree());
        }

        public static Imports FromCustomDebugInfo(
            CSharpCompilation compilation,
            Dictionary<string, AliasAndUsingDirective> usingAliases,
            ImmutableArray<NamespaceOrTypeAndUsingDirective> usings,
            ImmutableArray<AliasAndExternAliasDirective> externs)
        {
            return new Eager(compilation, usingAliases, usings, externs, ImmutableArray<Diagnostic>.Empty);
        }

        private static ImmutableArray<AliasAndExternAliasDirective> BuildExternAliases(
            SyntaxList<ExternAliasDirectiveSyntax> syntaxList,
            InContainerBinder binder,
            DiagnosticBag diagnostics)
        {
            CSharpCompilation compilation = binder.Compilation;

            var builder = ArrayBuilder<AliasAndExternAliasDirective>.GetInstance();

            foreach (ExternAliasDirectiveSyntax aliasSyntax in syntaxList)
            {
                compilation.RecordImport(aliasSyntax);

                // Extern aliases not allowed in interactive submissions:
                if (compilation.IsSubmission)
                {
                    diagnostics.Add(ErrorCode.ERR_ExternAliasNotAllowed, aliasSyntax.Location);
                    continue;
                }

                // some n^2 action, but n should be very small.
                foreach (var existingAlias in builder)
                {
                    if (existingAlias.Alias.Name == aliasSyntax.Identifier.ValueText)
                    {
                        diagnostics.Add(ErrorCode.ERR_DuplicateAlias, existingAlias.Alias.Locations[0], existingAlias.Alias.Name);
                        break;
                    }
                }

                if (aliasSyntax.Identifier.ContextualKind() == SyntaxKind.GlobalKeyword)
                {
                    diagnostics.Add(ErrorCode.ERR_GlobalExternAlias, aliasSyntax.Identifier.GetLocation());
                }

                builder.Add(new AliasAndExternAliasDirective(new AliasSymbol(binder, aliasSyntax), aliasSyntax));
            }

            return builder.ToImmutableAndFree();
        }

        public Dictionary<string, AliasAndUsingDirective> GetUsingAliases(ConsList<Symbol> basesBeingResolved, BinderFlags flags)
        {
            return (flags.Includes(BinderFlags.IgnoreUsings) ? Empty : this).GetUsingAliasesInternal(basesBeingResolved);
        }

        public ImmutableArray<NamespaceOrTypeAndUsingDirective> GetUsings(ConsList<Symbol> basesBeingResolved, BinderFlags flags)
        {
            return (flags.Includes(BinderFlags.IgnoreUsings) ? Empty : this).GetUsingsInternal(basesBeingResolved);
        }

        private void MarkImportDirective(CSharpSyntaxNode directive, bool callerIsSemanticModel)
        {
            MarkImportDirective(_compilation, directive, callerIsSemanticModel);
        }

        private static void MarkImportDirective(CSharpCompilation compilation, CSharpSyntaxNode directive, bool callerIsSemanticModel)
        {
            if (directive != null && compilation != null && !callerIsSemanticModel)
            {
                compilation.MarkImportDirectiveAsUsed(directive);
            }
        }

        internal void Complete(CancellationToken cancellationToken)
        {
            while (true)
            {
                cancellationToken.ThrowIfCancellationRequested();
                var incompletePart = _state.NextIncompletePart;
                switch (incompletePart)
                {
                    case CompletionPart.StartValidatingImports:
                        {
                            if (_state.NotePartComplete(CompletionPart.StartValidatingImports))
                            {
                                Validate();
                                _state.NotePartComplete(CompletionPart.FinishValidatingImports);
                            }
                        }
                        break;

                    case CompletionPart.FinishValidatingImports:
                        // some other thread has started validating imports (otherwise we would be in the case above) so
                        // we just wait for it to both finish and report the diagnostics.
                        Debug.Assert(_state.HasComplete(CompletionPart.StartValidatingImports));
                        _state.SpinWaitComplete(CompletionPart.FinishValidatingImports, cancellationToken);
                        break;

                    case CompletionPart.None:
                        return;

                    default:
                        // any other values are completion parts intended for other kinds of symbols
                        _state.NotePartComplete(CompletionPart.All & ~CompletionPart.ImportsAll);
                        break;
                }

                _state.SpinWaitComplete(incompletePart, cancellationToken);
            }
        }

        private void Validate()
        {
            DiagnosticBag semanticDiagnostics = _compilation.DeclarationDiagnostics;

            var usingAliases = GetUsingAliases(basesBeingResolved: null, flags: BinderFlags.None);

            if (usingAliases != null)
            {
                // Check constraints within named aliases.

                // Force resolution of named aliases.
                foreach (var alias in usingAliases.Values)
                {
                    alias.Alias.GetAliasTarget(basesBeingResolved: null);
                    semanticDiagnostics.AddRange(alias.Alias.AliasTargetDiagnostics);
                }

                foreach (var alias in usingAliases.Values)
                {
                    alias.Alias.CheckConstraints(semanticDiagnostics);
                }
            }

            // Force resolution of extern aliases.
            foreach (var alias in ExternAliases)
            {
                alias.Alias.GetAliasTarget(null);
                semanticDiagnostics.AddRange(alias.Alias.AliasTargetDiagnostics);
            }

            semanticDiagnostics.AddRange(Diagnostics);
        }

        internal bool IsUsingAlias(string name, ConsList<Symbol> basesBeingResolved, BinderFlags flags) // TODO (acasey): lazy
        {
            var usingAliases = GetUsingAliases(basesBeingResolved, flags);

            AliasAndUsingDirective node;
            if (usingAliases != null && usingAliases.TryGetValue(name, out node))
            {
                // This method is called by InContainerBinder.LookupSymbolsInSingleBinder to see if
                // there's a conflict between an alias and a member.  As a conflict may cause a
                // speculative lambda binding to fail this is semantically relevant and we need to
                // mark this using alias as referenced (and thus not something that can be removed).
                MarkImportDirective(node.UsingDirective, callerIsSemanticModel: flags.Includes(BinderFlags.SemanticModel));
                return true;
            }

            return false;
        }

        internal void LookupSymbol(
            Binder originalBinder,
            LookupResult result,
            string name,
            int arity,
            ConsList<Symbol> basesBeingResolved,
            LookupOptions options,
            bool diagnose,
            ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            LookupSymbolInAliases(originalBinder, result, name, arity, basesBeingResolved, options, diagnose, ref useSiteDiagnostics);

            if (!result.IsMultiViable && (options & LookupOptions.NamespaceAliasesOnly) == 0)
            {
                LookupSymbolInUsings(GetUsings(basesBeingResolved, originalBinder.Flags), originalBinder, result, name, arity, basesBeingResolved, options, diagnose, ref useSiteDiagnostics);
            }
        }

        internal void LookupSymbolInAliases(
            Binder originalBinder,
            LookupResult result,
            string name,
            int arity,
            ConsList<Symbol> basesBeingResolved,
            LookupOptions options,
            bool diagnose,
            ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            bool callerIsSemanticModel = originalBinder.IsSemanticModelBinder;

            var usingAliases = GetUsingAliases(basesBeingResolved, originalBinder.Flags);
            AliasAndUsingDirective alias;
            if (usingAliases != null && usingAliases.TryGetValue(name, out alias))
            {
                // Found a match in our list of normal aliases.  Mark the alias as being seen so that
                // it won't be reported to the user as something that can be removed.
                var res = originalBinder.CheckViability(alias.Alias, arity, options, null, diagnose, ref useSiteDiagnostics, basesBeingResolved);
                if (res.Kind == LookupResultKind.Viable)
                {
                    MarkImportDirective(alias.UsingDirective, callerIsSemanticModel);
                }

                result.MergeEqual(res);
            }

            foreach (var a in this.ExternAliases)
            {
                if (a.Alias.Name == name)
                {
                    // Found a match in our list of extern aliases.  Mark the extern alias as being
                    // seen so that it won't be reported to the user as something that can be
                    // removed.
                    var res = originalBinder.CheckViability(a.Alias, arity, options, null, diagnose, ref useSiteDiagnostics, basesBeingResolved);
                    if (res.Kind == LookupResultKind.Viable)
                    {
                        MarkImportDirective(a.ExternAliasDirective, callerIsSemanticModel);
                    }

                    result.MergeEqual(res);
                }
            }
        }

        internal static void LookupSymbolInUsings(
            ImmutableArray<NamespaceOrTypeAndUsingDirective> usings,
            Binder originalBinder,
            LookupResult result,
            string name,
            int arity,
            ConsList<Symbol> basesBeingResolved,
            LookupOptions options,
            bool diagnose,
            ref HashSet<DiagnosticInfo> useSiteDiagnostics)
        {
            bool callerIsSemanticModel = originalBinder.IsSemanticModelBinder;

            foreach (var typeOrNamespace in usings)
            {
                ImmutableArray<Symbol> candidates = Binder.GetCandidateMembers(typeOrNamespace.NamespaceOrType, name, options, originalBinder: originalBinder);
                foreach (Symbol symbol in candidates)
                {
                    switch (symbol.Kind)
                    {
                        // lookup via "using namespace" ignores namespaces inside
                        case SymbolKind.Namespace:
                            continue;

                        // lookup via "using static" ignores extension methods and non-static methods
                        case SymbolKind.Method:
                            if (!symbol.IsStatic || ((MethodSymbol)symbol).IsExtensionMethod)
                            {
                                continue;
                            }

                            break;

                        // types are considered static members for purposes of "using static" feature
                        // regardless of whether they are declared with "static" modifier or not
                        case SymbolKind.NamedType:
                            break;

                        // lookup via "using static" ignores non-static members
                        default:
                            if (!symbol.IsStatic)
                            {
                                continue;
                            }

                            break;
                    }

                    // Found a match in our list of normal using directives.  Mark the directive
                    // as being seen so that it won't be reported to the user as something that
                    // can be removed.
                    var res = originalBinder.CheckViability(symbol, arity, options, null, diagnose, ref useSiteDiagnostics, basesBeingResolved);
                    if (res.Kind == LookupResultKind.Viable)
                    {
                        MarkImportDirective(originalBinder.Compilation, typeOrNamespace.UsingDirective, callerIsSemanticModel);
                    }

                    result.MergeEqual(res);
                }
            }
        }

        internal void LookupExtensionMethodsInUsings(
            ArrayBuilder<MethodSymbol> methods,
            string name,
            int arity,
            LookupOptions options,
            BinderFlags flags)
        {
            Debug.Assert(methods.Count == 0);

            bool callerIsSemanticModel = flags.Includes(BinderFlags.SemanticModel);

            // We need to avoid collecting multiple candidates for an extension method imported both through a namespace and a static class
            // We will look for duplicates only if both of the following flags are set to true
            bool seenNamespaceWithExtensionMethods = false;
            bool seenStaticClassWithExtensionMethods = false;

            foreach (var @using in GetUsings(basesBeingResolved: null, flags: flags))
            {
                switch (@using.NamespaceOrType.Kind)
                {
                    case SymbolKind.Namespace:
                        {
                            var count = methods.Count;
                            ((NamespaceSymbol)@using.NamespaceOrType).GetExtensionMethods(methods, name, arity, options);

                            // If we found any extension methods, then consider this using as used.
                            if (methods.Count != count)
                            {
                                MarkImportDirective(@using.UsingDirective, callerIsSemanticModel);
                                seenNamespaceWithExtensionMethods = true;
                            }

                            break;
                        }

                    case SymbolKind.NamedType:
                        {
                            var count = methods.Count;
                            ((NamedTypeSymbol)@using.NamespaceOrType).GetExtensionMethods(methods, name, arity, options);

                            // If we found any extension methods, then consider this using as used.
                            if (methods.Count != count)
                            {
                                MarkImportDirective(@using.UsingDirective, callerIsSemanticModel);
                                seenStaticClassWithExtensionMethods = true;
                            }

                            break;
                        }
                }
            }

            if (seenNamespaceWithExtensionMethods && seenStaticClassWithExtensionMethods)
            {
                var methodsNoDuplicates = ArrayBuilder<MethodSymbol>.GetInstance();
                methodsNoDuplicates.AddRange(methods.Distinct());
                if (methodsNoDuplicates.Count < methods.Count)
                {
                    methods.Clear();
                    methods.AddRange(methodsNoDuplicates);
                }

                methodsNoDuplicates.Free();
            }
        }

        // Note: we do not mark nodes when looking up arities or names.  This is because these two
        // types of lookup are only around to make the public
        // SemanticModel.LookupNames/LookupSymbols work and do not count as usages of the directives
        // when the actual code is bound.

        internal void AddLookupSymbolsInfoInAliases(Binder binder, LookupSymbolsInfo result, LookupOptions options, Binder originalBinder)
        {
            var usingAliases = GetUsingAliases(basesBeingResolved: null, flags: originalBinder.Flags);
            if (usingAliases != null)
            {
                foreach (var usingAlias in usingAliases.Values)
                {
                    var usingAliasSymbol = usingAlias.Alias;
                    var usingAliasTargetSymbol = usingAliasSymbol.GetAliasTarget(basesBeingResolved: null);
                    if (originalBinder.CanAddLookupSymbolInfo(usingAliasTargetSymbol, options, null))
                    {
                        result.AddSymbol(usingAliasSymbol, usingAliasSymbol.Name, 0);
                    }
                }
            }

            if (this.ExternAliases != null)
            {
                foreach (var externAlias in this.ExternAliases)
                {
                    var externAliasSymbol = externAlias.Alias;
                    var externAliasTargetSymbol = externAliasSymbol.GetAliasTarget(basesBeingResolved: null);
                    if (originalBinder.CanAddLookupSymbolInfo(externAliasTargetSymbol, options, null))
                    {
                        result.AddSymbol(externAliasSymbol, externAliasSymbol.Name, 0);
                    }
                }
            }
        }

        internal static void AddLookupSymbolsInfoInUsings(
            ImmutableArray<NamespaceOrTypeAndUsingDirective> usings, Binder originalBinder, LookupSymbolsInfo result, LookupOptions options)
        {
            Debug.Assert(!options.CanConsiderNamespaces());

            // look in all using namespaces
            foreach (var namespaceSymbol in usings)
            {
                foreach (var member in namespaceSymbol.NamespaceOrType.GetMembersUnordered())
                {
                    if (originalBinder.CanAddLookupSymbolInfo(member, options, null))
                    {
                        result.AddSymbol(member, member.Name, member.GetArity());
                    }
                }
            }
        }
    }
}
