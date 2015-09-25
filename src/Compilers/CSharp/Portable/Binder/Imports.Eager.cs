// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// Represents symbols imported to the binding scope via using namespace, using alias, and extern alias.
    /// </summary>
    internal abstract partial class Imports
    {
        private sealed class Eager : Imports
        {
            private readonly Dictionary<string, AliasAndUsingDirective> _usingAliasesInternal;
            private readonly ImmutableArray<NamespaceOrTypeAndUsingDirective> _usingsInternal;

            protected override ImmutableArray<Diagnostic> Diagnostics { get; }

            public Eager(
                CSharpCompilation compilation,
                Dictionary<string, AliasAndUsingDirective> usingAliases,
                ImmutableArray<NamespaceOrTypeAndUsingDirective> usings,
                ImmutableArray<AliasAndExternAliasDirective> externs,
                ImmutableArray<Diagnostic> diagnostics)
                : base(externs, compilation)
            {
                Debug.Assert(!usings.IsDefault);
                Debug.Assert(!diagnostics.IsDefault);

                this._usingAliasesInternal = usingAliases;
                this._usingsInternal = usings;
                this.Diagnostics = diagnostics;
            }

            protected override Dictionary<string, AliasAndUsingDirective> GetUsingAliasesInternal(ConsList<Symbol> basesBeingResolved) => _usingAliasesInternal;
            protected override ImmutableArray<NamespaceOrTypeAndUsingDirective> GetUsingsInternal(ConsList<Symbol> basesBeingResolved) => _usingsInternal;

            protected override bool TryGetUsingAliasSyntax(string name, out UsingDirectiveSyntax syntax)
            {
                AliasAndUsingDirective node;
                if (_usingAliasesInternal != null && _usingAliasesInternal.TryGetValue(name, out node))
                {
                    syntax = node.UsingDirective;
                    return true;
                }

                syntax = null;
                return false;
            }
        }
    }
}