// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Microsoft.CodeAnalysis.ExpressionEvaluator;
using Microsoft.VisualStudio.Debugger.Clr;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Roslyn.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests
{
    public class FullNameTests : CSharpResultProviderTestBase
    {
        [Fact]
        public void RootComment()
        {
            var source = @"
class C
{
    public int F;
}
";
            var assembly = GetAssembly(source);
            var type = assembly.GetType("C");
            var value = CreateDkmClrValue(type.Instantiate());

            var root = FormatResult("a // Comment", value);
            Assert.Equal("a.F", GetChildren(root).Single().FullName);

            root = FormatResult(" a // Comment", value);
            Assert.Equal("a.F", GetChildren(root).Single().FullName);

            root = FormatResult("a// Comment", value);
            Assert.Equal("a.F", GetChildren(root).Single().FullName);

            root = FormatResult("a /*b*/ +c /*d*/// Comment", value);
            Assert.Equal("(a  +c).F", GetChildren(root).Single().FullName);

            root = FormatResult("a /*//*/+ c// Comment", value);
            Assert.Equal("(a + c).F", GetChildren(root).Single().FullName);

            root = FormatResult("a /*/**/+ c// Comment", value);
            Assert.Equal("(a + c).F", GetChildren(root).Single().FullName);

            root = FormatResult("/**/a// Comment", value);
            Assert.Equal("a.F", GetChildren(root).Single().FullName);
        }

        [Fact]
        public void RootFormatSpecifiers()
        {
            var source = @"
class C
{
    public int F;
}
";
            var assembly = GetAssembly(source);
            var type = assembly.GetType("C");
            var value = CreateDkmClrValue(type.Instantiate());

            var root = FormatResult("a, raw", value); // simple
            Assert.Equal("a, raw", root.FullName);
            Assert.Equal("a.F", GetChildren(root).Single().FullName);

            root = FormatResult("a, raw, ac, h", value); // multiple specifiers
            Assert.Equal("a, raw, ac, h", root.FullName);
            Assert.Equal("a.F", GetChildren(root).Single().FullName);

            root = FormatResult("M(a, b), raw", value); // non-specifier comma
            Assert.Equal("M(a, b), raw", root.FullName);
            Assert.Equal("(M(a, b)).F", GetChildren(root).Single().FullName); // parens not required

            root = FormatResult("a, raw1", value); // alpha-numeric
            Assert.Equal("a, raw1", root.FullName);
            Assert.Equal("a.F", GetChildren(root).Single().FullName);

            root = FormatResult("a, $raw", value); // other punctuation
            Assert.Equal("a, $raw", root.FullName);
            Assert.Equal("(a, $raw).F", GetChildren(root).Single().FullName); // not ideal
        }

        [Fact]
        public void RootParentheses()
        {
            var source = @"
class C
{
    public int F;
}
";
            var assembly = GetAssembly(source);
            var type = assembly.GetType("C");
            var value = CreateDkmClrValue(type.Instantiate());

            var root = FormatResult("a + b", value);
            Assert.Equal("(a + b).F", GetChildren(root).Single().FullName); // required

            root = FormatResult("new C()", value);
            Assert.Equal("(new C()).F", GetChildren(root).Single().FullName); // documentation

            root = FormatResult("A.B", value);
            Assert.Equal("A.B.F", GetChildren(root).Single().FullName); // desirable

            root = FormatResult("A::B", value);
            Assert.Equal("(A::B).F", GetChildren(root).Single().FullName); // documentation
        }

        [Fact]
        public void RootTrailingSemicolons()
        {
            var source = @"
class C
{
    public int F;
}
";
            var assembly = GetAssembly(source);
            var type = assembly.GetType("C");
            var value = CreateDkmClrValue(type.Instantiate());

            var root = FormatResult("a;", value);
            Assert.Equal("a.F", GetChildren(root).Single().FullName);

            root = FormatResult("a + b;;", value);
            Assert.Equal("(a + b).F", GetChildren(root).Single().FullName);

            root = FormatResult(" M( ) ; ;", value);
            Assert.Equal("(M( )).F", GetChildren(root).Single().FullName);
        }

        [Fact]
        public void RootMixedExtras()
        {
            var source = @"
class C
{
    public int F;
}
";
            var assembly = GetAssembly(source);
            var type = assembly.GetType("C");
            var value = CreateDkmClrValue(type.Instantiate());

            // Semicolon, then comment.
            var root = FormatResult("a; //", value);
            Assert.Equal("a", root.FullName);

            // Comment, then semicolon.
            root = FormatResult("a // ;", value);
            Assert.Equal("a", root.FullName);

            // Semicolon, then format specifier.
            root = FormatResult("a;, ac", value);
            Assert.Equal("a, ac", root.FullName);

            // Format specifier, then semicolon.
            root = FormatResult("a, ac;", value);
            Assert.Equal("a, ac", root.FullName);

            // Comment, then format specifier.
            root = FormatResult("a//, ac", value);
            Assert.Equal("a", root.FullName);

            // Format specifier, then comment.
            root = FormatResult("a, ac //", value);
            Assert.Equal("a, ac", root.FullName);

            // Everything.
            root = FormatResult("/*A*/ a /*B*/ + /*C*/ b /*D*/ ; ; , ac /*E*/, raw // ;, hidden", value);
            Assert.Equal("a  +  b, ac, raw", root.FullName);
        }

        [Fact, WorkItem(1022165)]
        public void Keywords_Root()
        {
            var source = @"
class C
{
    void M()
    {
        int @namespace = 3;
    }
}
";
            var assembly = GetAssembly(source);
            var value = CreateDkmClrValue(3);

            var root = FormatResult("@namespace", value);
            Verify(root,
                EvalResult("@namespace", "3", "int", "@namespace"));

            value = CreateDkmClrValue(assembly.GetType("C").Instantiate());
            root = FormatResult("this", value);
            Verify(root,
                EvalResult("this", "{C}", "C", "this"));

            // Verify that keywords aren't escaped by the ResultProvider at the
            // root level (we would never expect to see "namespace" passed as a
            // resultName, but this check verifies that we leave them "as is").
            root = FormatResult("namespace", CreateDkmClrValue(new object()));
            Verify(root,
                EvalResult("namespace", "{object}", "object", "namespace"));
        }

        [Fact]
        public void Keywords_RuntimeType()
        {
            var source = @"
public class @struct
{
}

public class @namespace : @struct
{
    @struct m = new @if();
}

public class @if : @struct
{
}
";
            var assembly = GetAssembly(source);
            var type = assembly.GetType("namespace");
            var declaredType = assembly.GetType("struct");
            var value = CreateDkmClrValue(type.Instantiate(), type);

            var root = FormatResult("o", value, new DkmClrType((TypeImpl)declaredType));
            Verify(GetChildren(root),
                EvalResult("m", "{if}", "struct {if}", "((@namespace)o).m", DkmEvaluationResultFlags.None));
        }

        [Fact]
        public void Keywords_ProxyType()
        {
            var source = @"
using System.Diagnostics;

[DebuggerTypeProxy(typeof(@class))]
public class @struct
{
    public bool @true = false;
}

public class @class
{
    public bool @false = true;

    public @class(@struct s) { }
}
";
            var assembly = GetAssembly(source);
            var value = CreateDkmClrValue(assembly.GetType("struct").Instantiate());

            var root = FormatResult("o", value);
            var children = GetChildren(root);
            Verify(children,
                EvalResult("@false", "true", "bool", "new @class(o).@false", DkmEvaluationResultFlags.Boolean | DkmEvaluationResultFlags.BooleanTrue),
                EvalResult("Raw View", null, "", "o, raw", DkmEvaluationResultFlags.Expandable | DkmEvaluationResultFlags.ReadOnly, DkmEvaluationResultCategory.Data));

            var grandChildren = GetChildren(children.Last());
            Verify(grandChildren,
                EvalResult("@true", "false", "bool", "o.@true", DkmEvaluationResultFlags.Boolean));
        }

        [Fact]
        public void Keywords_MemberAccess()
        {
            var source = @"
public class @struct
{
    public int @true;
}
";
            var assembly = GetAssembly(source);
            var value = CreateDkmClrValue(assembly.GetType("struct").Instantiate());

            var root = FormatResult("o", value);
            Verify(GetChildren(root),
                EvalResult("@true", "0", "int", "o.@true"));
        }

        [Fact]
        public void Keywords_StaticMembers()
        {
            var source = @"
public class @struct
{
    public static int @true;
}
";
            var assembly = GetAssembly(source);
            var value = CreateDkmClrValue(assembly.GetType("struct").Instantiate());

            var root = FormatResult("o", value);
            var children = GetChildren(root);
            Verify(children,
                EvalResult("Static members", null, "", "@struct", DkmEvaluationResultFlags.Expandable | DkmEvaluationResultFlags.ReadOnly, DkmEvaluationResultCategory.Class));
            Verify(GetChildren(children.Single()),
                EvalResult("@true", "0", "int", "@struct.@true", DkmEvaluationResultFlags.None, DkmEvaluationResultCategory.Data, DkmEvaluationResultAccessType.Public));
        }

        [Fact]
        public void Keywords_ExplicitInterfaceImplementation()
        {
            var source = @"
namespace @namespace
{
    public interface @interface<T>
    {
        int @return { get; set; }
    }

    public class @class : @interface<@class>
    {
        int @interface<@class>.@return { get; set; }
    }
}
";
            var assembly = GetAssembly(source);
            var value = CreateDkmClrValue(assembly.GetType("namespace.class").Instantiate());

            var root = FormatResult("instance", value);
            Verify(GetChildren(root),
                EvalResult("@namespace.@interface<@namespace.@class>.@return", "0", "int", "((@namespace.@interface<@namespace.@class>)instance).@return"));
        }

        [Fact]
        public void MangledNames_MemberAccess()
        {
            var il = @"
.class interface private abstract auto ansi <>IMangled
{
  .method public hidebysig newslot abstract virtual 
          instance void  M() cil managed
  {
  } // end of method <>IMangled::M

} // end of class <>IMangled

.class private auto ansi beforefieldinit C
       extends [mscorlib]System.Object
       implements <>IMangled
{
  .method private hidebysig newslot virtual final 
          instance void  <>IMangled.M() cil managed
  {
    .override <>IMangled::M
    // Code size       2 (0x2)
    .maxstack  8
    IL_0000:  nop
    IL_0001:  ret
  } // end of method C::<>IMangled.M

  .method public hidebysig specialname rtspecialname 
          instance void  .ctor() cil managed
  {
    // Code size       7 (0x7)
    .maxstack  8
    IL_0000:  ldarg.0
    IL_0001:  call       instance void [mscorlib]System.Object::.ctor()
    IL_0006:  ret
  } // end of method C::.ctor

} // end of class C
";

            ImmutableArray<byte> assemblyBytes;
            ImmutableArray<byte> pdbBytes;
            CSharpTestBase.EmitILToArray(il, appendDefaultHeader: true, includePdb: false, assemblyBytes: out assemblyBytes, pdbBytes: out pdbBytes);
            var assembly = ReflectionUtilities.Load(assemblyBytes);

            var value = CreateDkmClrValue(assembly.GetType("C").Instantiate());

            var root = FormatResult("o", value);
            Verify(GetChildren(root),
                EvalResult("@true", "0", "int", "o.@true"));
        }

        [Fact]
        public void MangledNames_StaticMembers()
        {
            var il = @"
.class public auto ansi beforefieldinit '<>Mangled' extends [mscorlib]System.Object
{
  .field public static int32 x

  .method public hidebysig specialname rtspecialname instance void  .ctor() cil managed
  {
    ldarg.0
    call       instance void [mscorlib]System.Object::.ctor()
    ret
  }
}
";

            ImmutableArray<byte> assemblyBytes;
            ImmutableArray<byte> pdbBytes;
            CSharpTestBase.EmitILToArray(il, appendDefaultHeader: true, includePdb: false, assemblyBytes: out assemblyBytes, pdbBytes: out pdbBytes);
            var assembly = ReflectionUtilities.Load(assemblyBytes);

            var value = CreateDkmClrValue(assembly.GetType("<>Mangled").Instantiate());

            var root = FormatResult("o", value);
            var children = GetChildren(root);
            Verify(children,
                EvalResult("Static members", null, "", null, DkmEvaluationResultFlags.Expandable | DkmEvaluationResultFlags.ReadOnly, DkmEvaluationResultCategory.Class));
            Verify(GetChildren(children.Single()),
                EvalResult("x", "0", "int", null, DkmEvaluationResultFlags.None, DkmEvaluationResultCategory.Data, DkmEvaluationResultAccessType.Public));
        }

        [Fact]
        public void MangledNames_ExplicitInterfaceImplementation()
        {
            var il = @"
.class interface public abstract auto ansi 'I<>Mangled'
{
  .method public hidebysig newslot specialname abstract virtual 
          instance int32  get_P() cil managed
  {
  }

  .property instance int32 P()
  {
    .get instance int32 'I<>Mangled'::get_P()
  }
} // end of class 'I<>Mangled'

.class public auto ansi beforefieldinit C
       extends [mscorlib]System.Object
       implements 'I<>Mangled'
{
  .method private hidebysig newslot specialname virtual final 
          instance int32  'I<>Mangled.get_P'() cil managed
  {
    .override 'I<>Mangled'::get_P
    ldc.i4.1
    ret
  }

  .method public hidebysig specialname rtspecialname 
          instance void  .ctor() cil managed
  {
    ldarg.0
    call       instance void [mscorlib]System.Object::.ctor()
    ret
  }

  .property instance int32 'I<>Mangled.P'()
  {
    .get instance int32 C::'I<>Mangled.get_P'()
  }

  .property instance int32 P()
  {
    .get instance int32 C::'I<>Mangled.get_P'()
  }
} // end of class C
";

            ImmutableArray<byte> assemblyBytes;
            ImmutableArray<byte> pdbBytes;
            CSharpTestBase.EmitILToArray(il, appendDefaultHeader: true, includePdb: false, assemblyBytes: out assemblyBytes, pdbBytes: out pdbBytes);
            var assembly = ReflectionUtilities.Load(assemblyBytes);

            var value = CreateDkmClrValue(assembly.GetType("C").Instantiate());

            var root = FormatResult("instance", value);
            Verify(GetChildren(root),
                EvalResult("I<>Mangled.P", "1", "int", null, DkmEvaluationResultFlags.ReadOnly, DkmEvaluationResultCategory.Property, DkmEvaluationResultAccessType.Private),
                EvalResult("P", "1", "int", "instance.P", DkmEvaluationResultFlags.ReadOnly, DkmEvaluationResultCategory.Property, DkmEvaluationResultAccessType.Private));
        }
    }
}
