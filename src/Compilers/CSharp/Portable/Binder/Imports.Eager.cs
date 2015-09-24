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
            protected override Dictionary<string, AliasAndUsingDirective> UsingAliasesInternal { get; }
            protected override ImmutableArray<NamespaceOrTypeAndUsingDirective> UsingsInternal { get; }

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

                this.UsingAliasesInternal = usingAliases;
                this.UsingsInternal = usings;
                this.Diagnostics = diagnostics;
            }
        }
    }
}