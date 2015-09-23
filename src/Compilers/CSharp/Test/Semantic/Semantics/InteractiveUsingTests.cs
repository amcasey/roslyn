﻿// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Roslyn.Test.Utilities;
using Xunit;
using System.Collections.Generic;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests
{
    public class InteractiveUsingTests : CSharpTestBase
    {
        // TODO (acasey): #loaded usings
        // TODO (acasey): global usings
        // TODO (acasey): notes https://github.com/tmat/roslyn/tree/master/docs/specs 

        [Fact]
        public void Using()
        {
            var sub = CreateSubmission("using System;");
            sub.VerifyDiagnostics();

            Assert.Equal(SpecialType.System_String, GetSpeculativeType(sub, "String").SpecialType);
        }

        [Fact]
        public void Alias()
        {
            var sub = CreateSubmission("using I = System.Int32;");
            sub.VerifyDiagnostics();

            Assert.Equal(SpecialType.System_Int32, GetSpeculativeType(sub, "I").SpecialType);
        }

        [Fact]
        public void UsingStatic()
        {
            var sub = CreateSubmission("using static System.Environment;");
            sub.VerifyDiagnostics();

            Assert.Equal(SymbolKind.Property, GetSpeculativeSymbol(sub, "NewLine").Kind);
        }

        [Fact]
        public void GlobalUsings()
        {
            var sub1 = CreateSubmission(
                "Combine(Environment.NewLine, Environment.NewLine)",
                options: TestOptions.DebugDll.WithUsings("System", "System.IO.Path"));
            sub1.VerifyDiagnostics();

            // No global usings specified - expect to reuse previous.
            var sub2 = CreateSubmission(
                "Combine(Environment.NewLine, Environment.NewLine)",
                previous: sub1);
            sub2.VerifyDiagnostics();

            // Global usings specified - expect to append to previous.
            var sub3 = CreateSubmission(
                "new StringBuilder().Append(Combine(Environment.NewLine, Environment.NewLine))",
                previous: sub2,
                options: TestOptions.DebugDll.WithUsings("System.Text"));
            sub3.VerifyDiagnostics();
        }

        [WorkItem(4811, "https://github.com/dotnet/roslyn/issues/4811")]
        [Fact]
        public void AliasCurrentSubmission()
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

            Assert.Equal(typeSymbol, GetSpeculativeType(sub, "Type"));
        }

        [WorkItem(4811, "https://github.com/dotnet/roslyn/issues/4811")]
        [Fact]
        public void AliasPreviousSubmission()
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

            Assert.Equal(typeSymbol, GetSpeculativeType(sub4, "C1"));
        }

        [Fact]
        public void AliasUnqualified()
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
        public void AliasUnqualified_GlobalUsing()
        {
            const string source = @"
using I = Int32;
";
            var expectedDiagnostics = new[]
            {
                // (2,11): error CS0246: The type or namespace name 'Int32' could not be found (are you missing a using directive or an assembly reference?)
                // using I = Int32;
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Int32").WithArguments("Int32").WithLocation(2, 11)
            };

            var options = TestOptions.DebugDll.WithUsings("System");

            CreateCompilationWithMscorlib(source, options: options).GetDiagnostics().Where(d => d.Severity > DiagnosticSeverity.Hidden).Verify(expectedDiagnostics);
            CreateSubmission(source, options: options).GetDiagnostics().Verify(expectedDiagnostics);
        }

        [Fact]
        public void AliasOtherAlias()
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

        [Fact]
        public void AliasHiding()
        {
            var sub1 = CreateSubmission("using A = System.Int32;");
            Assert.Equal(SpecialType.System_Int32, GetSpeculativeType(sub1, "A").SpecialType);

            var sub2 = CreateSubmission("using A = System.Int16;", previous: sub1);
            Assert.Equal(SpecialType.System_Int16, GetSpeculativeType(sub2, "A").SpecialType);

            var sub3 = CreateSubmission("class A { }", previous: sub2);
            Assert.Equal(sub3.ScriptClass, GetSpeculativeType(sub3, "A").ContainingType);

            var sub4 = CreateSubmission("using A = System.Int64;", previous: sub3);
            Assert.Equal(SpecialType.System_Int64, GetSpeculativeType(sub4, "A").SpecialType);
        }

        [Fact]
        public void UsingStaticCurrentSubmission()
        {
            const string source = @"
using static Type;

class Type
{
    public static readonly int Field = 1;
}
";

            var sub = CreateSubmission(source);
            sub.VerifyDiagnostics();

            Assert.Equal(sub.ScriptClass.GetMember("Type"), GetSpeculativeSymbol(sub, "Field").ContainingType);
        }

        [Fact]
        public void UsingStaticPreviousSubmission()
        {
            var sub1 = CreateSubmission("class A { static int AA; }");
            var sub2 = CreateSubmission("class B { static int BB; }", previous: sub1);
            var sub3 = CreateSubmission("class C { static int CC; }", previous: sub2);

            CreateSubmission("using static A;", previous: sub3).VerifyDiagnostics();
            CreateSubmission("using static B;", previous: sub3).VerifyDiagnostics();

            var sub4 = CreateSubmission("using static C;", previous: sub3);
            sub4.VerifyDiagnostics();

            var typeSymbol = sub3.ScriptClass.GetMember("C");

            Assert.Equal(typeSymbol, GetSpeculativeSymbol(sub4, "CC").ContainingType);
        }

        [Fact]
        public void UsingStaticUnqualified()
        {
            const string source = @"
using static Path;
using System.IO;
";
            var expectedDiagnostics = new[]
            {
                // (2,14): error CS0246: The type or namespace name 'Path' could not be found (are you missing a using directive or an assembly reference?)
                // using static Path;
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Path").WithArguments("Path").WithLocation(2, 14)
            };

            CreateCompilationWithMscorlib(source).GetDiagnostics().Where(d => d.Severity > DiagnosticSeverity.Hidden).Verify(expectedDiagnostics);
            CreateSubmission(source).GetDiagnostics().Verify(expectedDiagnostics);
        }

        [Fact]
        public void UsingStaticUnqualified_GlobalUsing()
        {
            const string source = @"
using static Path;
";
            var expectedDiagnostics = new[]
            {
                // (2,14): error CS0246: The type or namespace name 'Path' could not be found (are you missing a using directive or an assembly reference?)
                // using static Path;
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "Path").WithArguments("Path").WithLocation(2, 14)
            };

            var options = TestOptions.DebugDll.WithUsings("System");

            CreateCompilationWithMscorlib(source, options: options).GetDiagnostics().Where(d => d.Severity > DiagnosticSeverity.Hidden).Verify(expectedDiagnostics);
            CreateSubmission(source, options: options).GetDiagnostics().Verify(expectedDiagnostics);
        }

        [Fact]
        public void DuplicateUsing_SameSubmission()
        {
            CreateSubmission("using System; using System;").VerifyDiagnostics(
                // (1,21): warning CS0105: The using directive for 'System' appeared previously in this namespace
                // using System; using System;
                Diagnostic(ErrorCode.WRN_DuplicateUsing, "System").WithArguments("System").WithLocation(1, 21));
        }

        [Fact]
        public void DuplicateUsing_DifferentSubmissions()
        {
            CreateSubmission("using System;", previous: CreateSubmission("using System;")).VerifyDiagnostics();
        }

        [Fact]
        void UsingsFromLoadedScript()
        {
            const string scriptSource = @"
using static System.IO.Path;
using System.IO;
using F = System.IO.File;

class C { }
";

            const string submissionSource = @"
#load ""a.csx""

System.Type t;

GetTempPath(); // using static not exposed
t = typeof(File); // using not exposed
t = typeof(F); // using alias not exposed

t = typeof(C); // declaration exposed
";

            var resolver = TestSourceReferenceResolver.Create(new Dictionary<string, string>
            {
                { "a.csx", scriptSource }
            });

            var compilation = CreateSubmission(
                submissionSource,
                options: TestOptions.DebugDll.WithSourceReferenceResolver(resolver));

            compilation.VerifyDiagnostics(
                // (6,1): error CS0103: The name 'GetTempPath' does not exist in the current context
                // GetTempPath(); // using static not exposed
                Diagnostic(ErrorCode.ERR_NameNotInContext, "GetTempPath").WithArguments("GetTempPath").WithLocation(6, 1),
                // (7,12): error CS0246: The type or namespace name 'File' could not be found (are you missing a using directive or an assembly reference?)
                // t = typeof(File); // using not exposed
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "File").WithArguments("File").WithLocation(7, 12),
                // (8,12): error CS0246: The type or namespace name 'F' could not be found (are you missing a using directive or an assembly reference?)
                // t = typeof(F); // using alias not exposed
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "F").WithArguments("F").WithLocation(8, 12));
        }

        [Fact]
        void UsingsToLoadedScript()
        {
            const string scriptSource = @"
System.Type t;

GetTempPath(); // using static not exposed
t = typeof(File); // using not exposed
t = typeof(F); // using alias not exposed

t = typeof(C); // declaration exposed
";

            const string submissionSource = @"
#load ""a.csx""

using static System.IO.Path;
using System.IO;
using F = System.IO.File;

class C { }
";

            var resolver = TestSourceReferenceResolver.Create(new Dictionary<string, string>
            {
                { "a.csx", scriptSource }
            });

            var compilation = CreateSubmission(
                submissionSource,
                options: TestOptions.DebugDll.WithSourceReferenceResolver(resolver));

            compilation.VerifyDiagnostics(
                // a.csx(4,1): error CS0103: The name 'GetTempPath' does not exist in the current context
                // GetTempPath(); // using static not exposed
                Diagnostic(ErrorCode.ERR_NameNotInContext, "GetTempPath").WithArguments("GetTempPath").WithLocation(4, 1),
                // a.csx(5,12): error CS0246: The type or namespace name 'File' could not be found (are you missing a using directive or an assembly reference?)
                // t = typeof(File); // using not exposed
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "File").WithArguments("File").WithLocation(5, 12),
                // a.csx(6,12): error CS0246: The type or namespace name 'F' could not be found (are you missing a using directive or an assembly reference?)
                // t = typeof(F); // using alias not exposed
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "F").WithArguments("F").WithLocation(6, 12));
        }

        [Fact]
        void GlobalUsingsToLoadedScript()
        {
            const string scriptSource = @"
System.Type t;

GetTempPath(); // global using static exposed
t = typeof(File); // global using exposed
";

            const string submissionSource = @"
#load ""a.csx""
";

            var resolver = TestSourceReferenceResolver.Create(new Dictionary<string, string>
            {
                { "a.csx", scriptSource }
            });

            var compilation = CreateSubmission(
                submissionSource,
                options: TestOptions.DebugDll.WithSourceReferenceResolver(resolver).WithUsings("System.IO", "System.IO.Path"));

            compilation.VerifyDiagnostics();
        }

        private static Symbol GetSpeculativeSymbol(CSharpCompilation comp, string name)
        {
            var tree = comp.SyntaxTrees.Single();
            var model = comp.GetSemanticModel(tree);
            return (Symbol)model.GetSpeculativeSymbolInfo(
                tree.Length,
                SyntaxFactory.IdentifierName(name),
                SpeculativeBindingOption.BindAsExpression).Symbol;
        }

        private static TypeSymbol GetSpeculativeType(CSharpCompilation comp, string name)
        {
            var tree = comp.SyntaxTrees.Single();
            var model = comp.GetSemanticModel(tree);
            return (TypeSymbol)model.GetSpeculativeTypeInfo(
                tree.Length, 
                SyntaxFactory.IdentifierName(name), 
                SpeculativeBindingOption.BindAsTypeOrNamespace).Type;
        }
    }
}