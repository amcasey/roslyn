// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Test.Utilities;
using System.Linq;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests.Symbols.Source
{
    public class UsingAliasTests : SemanticModelTestBase
    {
        [Fact]
        public void GetSemanticInfo()
        {
            var text =
@"using O = System.Object;

partial class A : O {}
partial class A : object {}
partial class A : System.Object {}
partial class A : Object {}
";
            var tree = Parse(text);
            var root = tree.GetCompilationUnitRoot() as CompilationUnitSyntax;
            var comp = CreateCompilationWithMscorlib(tree);

            var usingAlias = root.Usings[0];

            var a1 = root.Members[0] as TypeDeclarationSyntax;
            var a2 = root.Members[1] as TypeDeclarationSyntax;
            var a3 = root.Members[2] as TypeDeclarationSyntax;
            var a4 = root.Members[3] as TypeDeclarationSyntax;

            var base1 = a1.BaseList.Types[0].Type as TypeSyntax;
            var base2 = a2.BaseList.Types[0].Type as TypeSyntax;
            var base3 = a3.BaseList.Types[0].Type as TypeSyntax;
            var base4 = a4.BaseList.Types[0].Type as TypeSyntax;

            var model = comp.GetSemanticModel(tree);

            var info1 = model.GetSemanticInfoSummary(base1);
            Assert.NotNull(info1.Symbol);
            var alias1 = model.GetAliasInfo((IdentifierNameSyntax)base1);
            Assert.NotNull(alias1);
            Assert.Equal(SymbolKind.Alias, alias1.Kind);
            Assert.Equal("O", alias1.ToDisplayString());
            Assert.Equal("O=System.Object", alias1.ToDisplayString(format: SymbolDisplayFormat.TestFormat));
            Assert.Equal(info1.Symbol, alias1.Target);

            var info2 = model.GetSemanticInfoSummary(base2);
            Assert.NotNull(info2.Symbol);
            var b2 = info2.Symbol;
            Assert.Equal("System.Object", b2.ToDisplayString(format: SymbolDisplayFormat.TestFormat));
            Assert.Equal("System.Object", info2.Type.ToDisplayString(format: SymbolDisplayFormat.TestFormat));

            var info3 = model.GetSemanticInfoSummary(base3);
            Assert.NotNull(info3.Symbol);
            var b3 = info3.Symbol;
            Assert.Equal("System.Object", b3.ToDisplayString(format: SymbolDisplayFormat.TestFormat));
            Assert.Equal("System.Object", info3.Type.ToDisplayString(format: SymbolDisplayFormat.TestFormat));

            var info4 = model.GetSemanticInfoSummary(base4);
            Assert.Null(info4.Symbol); // no "using System;"
            Assert.Equal(0, info4.CandidateSymbols.Length);
            var alias4 = model.GetAliasInfo((IdentifierNameSyntax)base4);
            Assert.Null(alias4);
        }

        [Fact]
        public void GetSymbolInfoInParent()
        {
            var text =
@"using O = System.Object;

partial class A : O {}
partial class A : object {}
partial class A : System.Object {}
partial class A : Object {}
";
            var tree = Parse(text);
            var root = tree.GetCompilationUnitRoot() as CompilationUnitSyntax;
            var comp = CreateCompilationWithMscorlib(tree);

            var usingAlias = root.Usings[0];

            var a1 = root.Members[0] as TypeDeclarationSyntax;
            var a2 = root.Members[1] as TypeDeclarationSyntax;
            var a3 = root.Members[2] as TypeDeclarationSyntax;
            var a4 = root.Members[3] as TypeDeclarationSyntax;

            var base1 = a1.BaseList.Types[0].Type as TypeSyntax;
            var base2 = a2.BaseList.Types[0].Type as TypeSyntax;
            var base3 = a3.BaseList.Types[0].Type as TypeSyntax;
            var base4 = a4.BaseList.Types[0].Type as TypeSyntax;

            var model = comp.GetSemanticModel(tree);

            var info1 = model.GetSemanticInfoSummary(base1);
            Assert.Equal("System.Object", info1.Type.ToDisplayString(format: SymbolDisplayFormat.TestFormat));
            var alias1 = model.GetAliasInfo((IdentifierNameSyntax)base1);
            Assert.NotNull(alias1);
            Assert.Equal(SymbolKind.Alias, alias1.Kind);
            Assert.Equal("O=System.Object", alias1.ToDisplayString(format: SymbolDisplayFormat.TestFormat));

            var info2 = model.GetSemanticInfoSummary(base2);
            Assert.NotNull(info2.Symbol);
            var b2 = info2.Symbol;
            Assert.Equal("System.Object", b2.ToDisplayString(format: SymbolDisplayFormat.TestFormat));
            Assert.Equal("System.Object", info2.Type.ToDisplayString(format: SymbolDisplayFormat.TestFormat));

            var info3 = model.GetSemanticInfoSummary(base3);
            Assert.NotNull(info3.Symbol);
            var b3 = info3.Symbol;
            Assert.Equal("System.Object", b3.ToDisplayString(format: SymbolDisplayFormat.TestFormat));
            Assert.Equal("System.Object", info3.Type.ToDisplayString(format: SymbolDisplayFormat.TestFormat));

            var info4 = model.GetSemanticInfoSummary(base4);
            Assert.Null(info4.Symbol); // no "using System;"
            Assert.Equal(0, info4.CandidateSymbols.Length);
            var alias4 = model.GetAliasInfo((IdentifierNameSyntax)base4);
            Assert.Null(alias4);
        }

        [Fact]
        public void BindType()
        {
            var text =
@"using O = System.Object;

partial class A : O {}
partial class A : object {}
partial class A : System.Object {}
partial class A : Object {}
";
            var tree = Parse(text);
            var root = tree.GetCompilationUnitRoot() as CompilationUnitSyntax;
            var comp = CreateCompilationWithMscorlib(tree);

            var usingAlias = root.Usings[0];

            var a1 = root.Members[0] as TypeDeclarationSyntax;
            var a2 = root.Members[1] as TypeDeclarationSyntax;
            var a3 = root.Members[2] as TypeDeclarationSyntax;
            var a4 = root.Members[3] as TypeDeclarationSyntax;

            var base1 = a1.BaseList.Types[0].Type as TypeSyntax;
            var base2 = a2.BaseList.Types[0].Type as TypeSyntax;
            var base3 = a3.BaseList.Types[0].Type as TypeSyntax;
            var base4 = a4.BaseList.Types[0].Type as TypeSyntax;

            var model = comp.GetSemanticModel(tree);

            var symbolInfo = model.GetSpeculativeSymbolInfo(base2.SpanStart, base2, SpeculativeBindingOption.BindAsTypeOrNamespace);
            var info2 = symbolInfo.Symbol as TypeSymbol;
            Assert.NotNull(info2);
            Assert.Equal("System.Object", info2.ToDisplayString(format: SymbolDisplayFormat.TestFormat));
            Assert.Equal("System.Object", info2.ToDisplayString(format: SymbolDisplayFormat.TestFormat));

            symbolInfo = model.GetSpeculativeSymbolInfo(base3.SpanStart, base3, SpeculativeBindingOption.BindAsTypeOrNamespace);
            var info3 = symbolInfo.Symbol as TypeSymbol;
            Assert.NotNull(info3);
            Assert.Equal("System.Object", info3.ToDisplayString(format: SymbolDisplayFormat.TestFormat));
            Assert.Equal("System.Object", info3.ToDisplayString(format: SymbolDisplayFormat.TestFormat));

            symbolInfo = model.GetSpeculativeSymbolInfo(base4.SpanStart, base4, SpeculativeBindingOption.BindAsTypeOrNamespace);
            var info4 = symbolInfo.Symbol as TypeSymbol;
            Assert.Null(info4); // no "using System;"
        }

        [Fact]
        public void GetDeclaredSymbol01()
        {
            var text =
@"using O = System.Object;
";
            var tree = Parse(text);
            var root = tree.GetCompilationUnitRoot() as CompilationUnitSyntax;
            var comp = CreateCompilationWithMscorlib(tree);

            var usingAlias = root.Usings[0];

            var model = comp.GetSemanticModel(tree);

            var alias = model.GetDeclaredSymbol(usingAlias);
            Assert.Equal("O", alias.ToDisplayString());
            Assert.Equal("O=System.Object", alias.ToDisplayString(format: SymbolDisplayFormat.TestFormat));
            var global = (NamespaceSymbol)alias.ContainingSymbol;
            Assert.Equal(NamespaceKind.Module, global.Extent.Kind);
        }

        [Fact]
        public void GetDeclaredSymbol02()
        {
            var text = "using System;";
            var tree = Parse(text);
            var root = tree.GetCompilationUnitRoot() as CompilationUnitSyntax;
            var comp = CreateCompilationWithMscorlib(tree);

            var usingAlias = root.Usings[0];

            var model = comp.GetSemanticModel(tree);

            var alias = model.GetDeclaredSymbol(usingAlias);
            Assert.Equal(null, alias);
        }

        [Fact]
        public void LookupNames()
        {
            var text =
@"using O = System.Object;
class C {}
";
            var tree = Parse(text);
            var root = tree.GetCompilationUnitRoot() as CompilationUnitSyntax;
            var comp = CreateCompilationWithMscorlib(tree);

            var usingAlias = root.Usings[0];

            var model = comp.GetSemanticModel(tree);

            var names = model.LookupNames(root.Members[0].SpanStart);
            Assert.Contains("O", names);
        }

        [Fact]
        public void LookupSymbols()
        {
            var text =
@"using O = System.Object;
class C {}
";
            var tree = Parse(text);
            var root = tree.GetCompilationUnitRoot() as CompilationUnitSyntax;
            var comp = CreateCompilationWithMscorlib(tree);

            var usingAlias = root.Usings[0];

            var model = comp.GetSemanticModel(tree);

            var symbols = model.LookupSymbols(root.Members[0].SpanStart, name: "O");
            Assert.Equal(1, symbols.Length);
            Assert.Equal(SymbolKind.Alias, symbols[0].Kind);
            Assert.Equal("O=System.Object", symbols[0].ToDisplayString(format: SymbolDisplayFormat.TestFormat));
        }

        [WorkItem(537401, "DevDiv")]
        [Fact]
        public void EventEscapedIdentifier()
        {
            var text = @"
using @for = @foreach;
namespace @foreach { }
";
            SyntaxTree syntaxTree = Parse(text);
            CSharpCompilation comp = CreateCompilationWithMscorlib(syntaxTree);
            UsingDirectiveSyntax usingAlias = (syntaxTree.GetCompilationUnitRoot() as CompilationUnitSyntax).Usings.First();
            var alias = comp.GetSemanticModel(syntaxTree).GetDeclaredSymbol(usingAlias);
            Assert.Equal("for", alias.Name);
            Assert.Equal("@for", alias.ToString());
        }

        [WorkItem(541937, "DevDiv")]
        [Fact]
        public void LocalDeclaration()
        {
            var text = @"
using GIBBERISH = System.Int32;
class Program
{
    static void Main()
    {
        /*<bind>*/GIBBERISH/*</bind>*/ x;
    }
}";
            SyntaxTree syntaxTree = Parse(text);
            CSharpCompilation comp = CreateCompilationWithMscorlib(syntaxTree);
            var model = comp.GetSemanticModel(syntaxTree);
            IdentifierNameSyntax exprSyntaxToBind = (IdentifierNameSyntax)GetExprSyntaxForBinding(GetExprSyntaxList(syntaxTree));
            Assert.Equal(SymbolKind.Alias, model.GetAliasInfo(exprSyntaxToBind).Kind);
        }

        [WorkItem(576809, "DevDiv")]
        [Fact]
        public void AsClause()
        {
            var text = @"
using N = System.Nullable<int>;
 
class Program
{
    static void Main()
    {
        object x = 1;
        var y = x as /*<bind>*/N/*</bind>*/ + 1;
    }
}
";
            SyntaxTree syntaxTree = Parse(text);
            CSharpCompilation comp = CreateCompilationWithMscorlib(syntaxTree);
            var model = comp.GetSemanticModel(syntaxTree);
            IdentifierNameSyntax exprSyntaxToBind = (IdentifierNameSyntax)GetExprSyntaxForBinding(GetExprSyntaxList(syntaxTree));
            Assert.Equal("System.Int32?", model.GetAliasInfo(exprSyntaxToBind).Target.ToTestDisplayString());
        }

        [WorkItem(542552, "DevDiv")]
        [Fact]
        public void IncompleteDuplicateAlias()
        {
            var text = @"namespace namespace1 { }
namespace namespace2 { }
namespace prog
{
    using ns = namespace1;
    using ns =";
            SyntaxTree syntaxTree = Parse(text);
            CSharpCompilation comp = CreateCompilationWithMscorlib(syntaxTree);
            var discarded = comp.GetDiagnostics();
        }

        [ClrOnlyFact, WorkItem(2805, "https://github.com/dotnet/roslyn/issues/2805")]
        public void AliasWithAnError()
        {
            var text =
@"
namespace NS
{
    using Short = LongNamespace;
    class Test
    {
        public object Method1()
        {
            return (new Short.MyClass()).Prop;
        }
    }
}";

            var compilation = CreateCompilationWithMscorlib(text);

            compilation.VerifyDiagnostics(
    // (4,19): error CS0246: The type or namespace name 'LongNamespace' could not be found (are you missing a using directive or an assembly reference?)
    //     using Short = LongNamespace;
    Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "LongNamespace").WithArguments("LongNamespace").WithLocation(4, 19)
                );

            var tree = compilation.SyntaxTrees.Single();

            var node = tree.GetRoot().DescendantNodes().OfType<IdentifierNameSyntax>().Where(id => id.Identifier.ValueText == "Short").Skip(1).Single();

            Assert.Equal("Short.MyClass", node.Parent.ToString());

            var model = compilation.GetSemanticModel(tree);

            var alias = model.GetAliasInfo(node);
            Assert.Equal("Short=LongNamespace", alias.ToTestDisplayString());
            Assert.Equal(SymbolKind.ErrorType, alias.Target.Kind);
            Assert.Equal("LongNamespace", alias.Target.ToTestDisplayString());

            var symbolInfo = model.GetSymbolInfo(node);

            Assert.Null(symbolInfo.Symbol);
            Assert.Equal(0, symbolInfo.CandidateSymbols.Length);
            Assert.Equal(CandidateReason.None, symbolInfo.CandidateReason);
        }

        [WorkItem(4811, "https://github.com/dotnet/roslyn/issues/4811")]
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

        [WorkItem(4811, "https://github.com/dotnet/roslyn/issues/4811")]
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
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Int32").WithArguments("Int32").WithLocation(2, 11),
                // (3,1): hidden CS8019: Unnecessary using directive.
                // using System;
                Diagnostic(ErrorCode.HDN_UnusedUsingDirective, "using System;").WithLocation(3, 1),
                // (2,1): hidden CS8019: Unnecessary using directive.
                // using I = Int32;
                Diagnostic(ErrorCode.HDN_UnusedUsingDirective, "using I = Int32;").WithLocation(2, 1)
            };

            CreateCompilationWithMscorlib(source).VerifyDiagnostics(expectedDiagnostics);
            CreateSubmission(source).VerifyDiagnostics(expectedDiagnostics);
        }

        [WorkItem(4811, "https://github.com/dotnet/roslyn/issues/4811")]
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
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "I").WithArguments("I").WithLocation(3, 11),
                // (2,1): hidden CS8019: Unnecessary using directive.
                // using I = System.Int32;
                Diagnostic(ErrorCode.HDN_UnusedUsingDirective, "using I = System.Int32;").WithLocation(2, 1),
                // (3,1): hidden CS8019: Unnecessary using directive.
                // using J = I;
                Diagnostic(ErrorCode.HDN_UnusedUsingDirective, "using J = I;").WithLocation(3, 1)
            };

            CreateCompilationWithMscorlib(source).VerifyDiagnostics(expectedDiagnostics);
            CreateSubmission(source).VerifyDiagnostics(expectedDiagnostics);
        }
    }
}
