// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Roslyn.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests
{
    public class InteractiveUsingTests : CSharpTestBase
    {
        // TODO (acasey): using static
        // TODO (acasey): notes https://github.com/tmat/roslyn/tree/master/docs/specs 

        [Fact]
        public void UsingInSubmission()
        {
            CreateSubmission("using System;").VerifyDiagnostics();
        }

        [WorkItem(4811, "https://github.com/dotnet/roslyn/issues/4811")]
        [Fact]
        public void AliasInSubmission()
        {
            const string source = @"
using T = Type;

class Type { }
";

            var sub = CreateSubmission(source);
            sub.VerifyDiagnostics();

            var typeSymbol = sub.ScriptClass.GetMember("Type");

            var tree = sub.SyntaxTrees.Single();
            var model = sub.GetSemanticModel(tree);
            var syntax = tree.GetRoot().DescendantNodes().OfType<UsingDirectiveSyntax>().Single();

            var aliasSymbol = model.GetDeclaredSymbol(syntax);
            Assert.Equal(SymbolKind.Alias, aliasSymbol.Kind);
            Assert.Equal(typeSymbol, ((AliasSymbol)aliasSymbol).Target);

            Assert.Equal(typeSymbol, model.GetSymbolInfo(syntax.Name).Symbol);
        }

        [WorkItem(4811, "https://github.com/dotnet/roslyn/issues/4811")]
        [Fact]
        public void AliasInSubmission_PreviousSubmission()
        {
            var sub1 = CreateSubmission("class A { }");
            var sub2 = CreateSubmission("class B : A { }", previous: sub1);
            var sub3 = CreateSubmission("class C : B { }", previous: sub2);

            CreateSubmission("using A1 = A;", previous: sub3).VerifyDiagnostics();
            CreateSubmission("using B1 = B;", previous: sub3).VerifyDiagnostics();

            var sub4 = CreateSubmission("using C1 = C;", previous: sub3);
            sub4.VerifyDiagnostics();

            var typeSymbol = sub3.ScriptClass.GetMember("C");

            var tree = sub4.SyntaxTrees.Single();
            var model = sub4.GetSemanticModel(tree);
            var syntax = tree.GetRoot().DescendantNodes().OfType<UsingDirectiveSyntax>().Single();

            var aliasSymbol = model.GetDeclaredSymbol(syntax);
            Assert.Equal(SymbolKind.Alias, aliasSymbol.Kind);
            Assert.Equal(typeSymbol, ((AliasSymbol)aliasSymbol).Target);

            Assert.Equal(typeSymbol, model.GetSymbolInfo(syntax.Name).Symbol);
        }

        [Fact]
        public void AliasInSubmission_Unqualified()
        {
            const string source = @"
using I = Int32;
using System;
";
            var expectedDiagnostics = new[]
            {
                // (2,11): error CS0246: The type or namespace name 'Int32' could not be found (are you missing a using directive or an assembly reference?)
                // using I = Int32;
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Int32").WithArguments("Int32").WithLocation(2, 11)
            };

            CreateCompilationWithMscorlib(source).GetDiagnostics().Where(d => d.Severity > DiagnosticSeverity.Hidden).Verify(expectedDiagnostics);
            CreateSubmission(source).GetDiagnostics().Verify(expectedDiagnostics);
        }

        [Fact]
        public void AliasInSubmission_Chain()
        {
            const string source = @"
using I = System.Int32;
using J = I;
";
            var expectedDiagnostics = new[]
            {
                // (3,11): error CS0246: The type or namespace name 'I' could not be found (are you missing a using directive or an assembly reference?)
                // using J = I;
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "I").WithArguments("I").WithLocation(3, 11)
            };

            CreateCompilationWithMscorlib(source).GetDiagnostics().Where(d => d.Severity > DiagnosticSeverity.Hidden).Verify(expectedDiagnostics);
            CreateSubmission(source).GetDiagnostics().Verify(expectedDiagnostics);
        }
    }
}