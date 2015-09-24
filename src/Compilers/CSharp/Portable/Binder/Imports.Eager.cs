// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// Represents symbols imported to the binding scope via using namespace, using alias, and extern alias.
    /// </summary>
    internal abstract partial class Imports
    {
        private sealed class Eager : Imports
        {
            public override Dictionary<string, AliasAndUsingDirective> UsingAliases { get; }
            public override ImmutableArray<NamespaceOrTypeAndUsingDirective> Usings { get; }

            public Eager(
                CSharpCompilation compilation,
                Dictionary<string, AliasAndUsingDirective> usingAliases,
                ImmutableArray<NamespaceOrTypeAndUsingDirective> usings,
                ImmutableArray<AliasAndExternAliasDirective> externs,
                DiagnosticBag diagnostics)
                : base(externs, compilation, diagnostics)
            {
                Debug.Assert(!usings.IsDefault);

                this.UsingAliases = usingAliases;
                this.Usings = usings;
            }
        }
    }
}