// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Threading;
using Microsoft.CodeAnalysis.Collections;
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
        private sealed class Lazy : Imports
        {
            private readonly Binder _usingsBinder;
            private readonly SyntaxList<UsingDirectiveSyntax> _usingDirectives;
            private readonly ImmutableArray<Diagnostic> _externDiagnostics;

            private ResolvedUsings _lazyResolvedUsings;

            public Lazy(
                InContainerBinder binder,
                ImmutableArray<AliasAndExternAliasDirective> externs,
                SyntaxList<UsingDirectiveSyntax> usingDirectives,
                ImmutableArray<Diagnostic> externDiagnostics)
                : base(externs, binder.Compilation)
            {
                Debug.Assert(usingDirectives.Count > 0, "Should use eager Imports when there are no usings");

                // Usings are ignored while binding usings.
                _usingsBinder = binder.WithFlags(BinderFlags.IgnoreUsings);
                _usingDirectives = usingDirectives;
                _externDiagnostics = externDiagnostics;
            }

            protected override Dictionary<string, AliasAndUsingDirective> GetUsingAliasesInternal(ConsList<Symbol> basesBeingResolved) => 
                GetResolvedUsings(basesBeingResolved).UsingAliases;

            protected override ImmutableArray<NamespaceOrTypeAndUsingDirective> GetUsingsInternal(ConsList<Symbol> basesBeingResolved) =>
                GetResolvedUsings(basesBeingResolved).Usings;

            protected override ImmutableArray<Diagnostic> Diagnostics => GetResolvedUsings(basesBeingResolved: null).Diagnostics;

            protected override bool TryGetUsingAliasSyntax(string name, out UsingDirectiveSyntax syntax)
            {
                if (_lazyResolvedUsings != null)
                {
                    var usingAliases = _lazyResolvedUsings.UsingAliases;

                    AliasAndUsingDirective node;
                    if (usingAliases != null && usingAliases.TryGetValue(name, out node))
                    {
                        syntax = node.UsingDirective;
                        return true;
                    }

                    syntax = null;
                    return false;
                }

                foreach (var directive in _usingDirectives)
                {
                    if (directive.Alias?.Name.Identifier.ValueText == name)
                    {
                        syntax = directive;
                        return true;
                    }
                }

                syntax = null;
                return false;
            }

            private ResolvedUsings GetResolvedUsings(ConsList<Symbol> basesBeingResolved)
            {
                if (_lazyResolvedUsings == null)
                {
                    Interlocked.CompareExchange(ref _lazyResolvedUsings, ResolveUsings(basesBeingResolved), null);
                }

                return _lazyResolvedUsings;
            }

            private ResolvedUsings ResolveUsings(ConsList<Symbol> basesBeingResolved)
            {
                var compilation = _usingsBinder.Compilation;

                var usings = ArrayBuilder<NamespaceOrTypeAndUsingDirective>.GetInstance();
                Dictionary<string, AliasAndUsingDirective> usingAliases = null;

                var uniqueUsings = PooledHashSet<NamespaceOrTypeSymbol>.GetInstance();

                var diagnostics = DiagnosticBag.GetInstance();
                diagnostics.AddRange(_externDiagnostics);

                foreach (var usingDirective in _usingDirectives)
                {
                    compilation.RecordImport(usingDirective);

                    if (usingDirective.Alias != null)
                    {
                        if (usingDirective.StaticKeyword != default(SyntaxToken))
                        {
                            diagnostics.Add(ErrorCode.ERR_NoAliasHere, usingDirective.Alias.Name.Location);
                        }

                        string identifierValueText = usingDirective.Alias.Name.Identifier.ValueText;
                        if (usingAliases != null && usingAliases.ContainsKey(identifierValueText))
                        {
                            // Suppress diagnostics if we're already broken.
                            if (!usingDirective.Name.IsMissing)
                            {
                                // The using alias '{0}' appeared previously in this namespace
                                diagnostics.Add(ErrorCode.ERR_DuplicateAlias, usingDirective.Alias.Name.Location, identifierValueText);
                            }
                        }
                        else
                        {
                            // an O(m*n) algorithm here but n (number of extern aliases) will likely be very small.
                            foreach (var externAlias in ExternAliases)
                            {
                                if (externAlias.Alias.Name == identifierValueText)
                                {
                                    // The using alias '{0}' appeared previously in this namespace
                                    diagnostics.Add(ErrorCode.ERR_DuplicateAlias, usingDirective.Location, identifierValueText);
                                    break;
                                }
                            }

                            if (usingAliases == null)
                            {
                                usingAliases = new Dictionary<string, AliasAndUsingDirective>();
                            }

                            // construct the alias sym with the binder for which we are building imports. That
                            // way the alias target can make use of extern alias definitions.
                            usingAliases.Add(identifierValueText, new AliasAndUsingDirective(new AliasSymbol(_usingsBinder, usingDirective), usingDirective));
                        }
                    }
                    else
                    {
                        if (usingDirective.Name.IsMissing)
                        {
                            //don't try to lookup namespaces inserted by parser error recovery
                            continue;
                        }

                        var imported = _usingsBinder.BindNamespaceOrTypeSymbol(usingDirective.Name, diagnostics, basesBeingResolved);
                        if (imported.Kind == SymbolKind.Namespace)
                        {
                            if (usingDirective.StaticKeyword != default(SyntaxToken))
                            {
                                diagnostics.Add(ErrorCode.ERR_BadUsingType, usingDirective.Name.Location, imported);
                            }
                            else if (uniqueUsings.Contains(imported))
                            {
                                diagnostics.Add(ErrorCode.WRN_DuplicateUsing, usingDirective.Name.Location, imported);
                            }
                            else
                            {
                                uniqueUsings.Add(imported);
                                usings.Add(new NamespaceOrTypeAndUsingDirective(imported, usingDirective));
                            }
                        }
                        else if (imported.Kind == SymbolKind.NamedType)
                        {
                            if (usingDirective.StaticKeyword == default(SyntaxToken))
                            {
                                diagnostics.Add(ErrorCode.ERR_BadUsingNamespace, usingDirective.Name.Location, imported);
                            }
                            else
                            {
                                var importedType = (NamedTypeSymbol)imported;
                                if (uniqueUsings.Contains(importedType))
                                {
                                    diagnostics.Add(ErrorCode.WRN_DuplicateUsing, usingDirective.Name.Location, importedType);
                                }
                                else
                                {
                                    uniqueUsings.Add(importedType);
                                    usings.Add(new NamespaceOrTypeAndUsingDirective(importedType, usingDirective));
                                }
                            }
                        }
                        else if (imported.Kind != SymbolKind.ErrorType)
                        {
                            // Do not report additional error if the symbol itself is erroneous.

                            // error: '<symbol>' is a '<symbol kind>' but is used as 'type or namespace'
                            diagnostics.Add(ErrorCode.ERR_BadSKknown, usingDirective.Name.Location,
                                usingDirective.Name,
                                imported.GetKindText(),
                                MessageID.IDS_SK_TYPE_OR_NAMESPACE.Localize());
                        }
                    }
                }

                uniqueUsings.Free();

                return new ResolvedUsings(usingAliases, usings.ToImmutableAndFree(), diagnostics.ToReadOnlyAndFree());
            }

            private class ResolvedUsings
            {
                public readonly Dictionary<string, AliasAndUsingDirective> UsingAliases;
                public readonly ImmutableArray<NamespaceOrTypeAndUsingDirective> Usings;
                public readonly ImmutableArray<Diagnostic> Diagnostics;

                public ResolvedUsings(
                    Dictionary<string, AliasAndUsingDirective> usingAliases,
                    ImmutableArray<NamespaceOrTypeAndUsingDirective> usings,
                    ImmutableArray<Diagnostic> diagnostics)
                {
                    UsingAliases = usingAliases;
                    Usings = usings;
                    Diagnostics = diagnostics;
                }
            }
        }
    }
}