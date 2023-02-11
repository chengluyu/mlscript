package mlscript.codegen.generator

import mlscript.codegen.ast._
import mlscript.codegen.generator._

class CodeGenerationSuite extends munit.FunSuite:
  private val format = new Format {
    val compact: Boolean = false
    val minified: Boolean = false
    var concise: Boolean = false
    val retainLines: Boolean = false
    val auxiliaryCommentBefore: String = ""
    val auxiliaryCommentAfter: String = ""
    val adjustMultilineComment: Boolean = false
    val retainFunctionParens: Boolean = false
    val indent = "  "
    
    def shouldPrintComment(comment: String): Boolean = false
  }

  private val sourceMap = new SourceMapBuilder(None, None, Left(""))

  test("Basic Test") {
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.print(Import()(None, None, None))(0)
      assertEquals(generator.get.code, "import")
    }
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.print(EmptyStatement()(None, None, None))(0)
      assertEquals(generator.get.code, ";")
    }
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.print(StringLiteral("abc")(None, None, None))(0)
      assertEquals(generator.get.code, "\"abc\"")
    }
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.print(NumericLiteral(114.514)(None, None, None))(0)
      assertEquals(generator.get.code, "114.514")
    }
  }

  test("Composed Test") {
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.print(ImportSpecifier(Some(Identifier("bar")(None, None, None)), Identifier("foo")(None, None, None), ImportKind.Value)(None, None, None))(0)
      assertEquals(generator.get.code, "foo as bar")
    }
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.print(ReturnStatement(Some(BinaryExpression(
        BinaryOperator.Plus, Identifier("foo")(None, None, None), Identifier("bar")(None, None, None)
      )(None, None, None)))(None, None, None))(0)
      assertEquals(generator.get.code, "return foo + bar;")
    }
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.print(
        OptionalCallExpression(
          Identifier("foo")(None, None, None),
          List(NumericLiteral(114)(None, None, None), NumericLiteral(514)(None, None, None),
            NumericLiteral(1919)(None, None, None), NumericLiteral(810)(None, None, None)),
          true
        )(None, None, None)
      )(0)
      assertEquals(generator.get.code, "foo?.(114, 514, 1919, 810)")
    }
  }
  test("Comprehensive Test") {
    val generator = CodeGenerator(format, sourceMap)
    generator.print(File(
      Program(
        List(
          ClassDeclaration(
            Identifier("option")(None, None, None), None,
            ClassBody(List(
              ClassProperty(Identifier("type")(None, None, None))(None, None, None)
            ))(None, None, None)
          )(None, None, None),

          ClassDeclaration(
            Identifier("some")(None, None, None), Some(Identifier("option")(None, None, None)),
            ClassBody(List(
              ClassProperty(Identifier("value")(None, None, None))(None, None, None),
              ClassMethod(
                ClassMethodKind.Constructor, Identifier("constructor")(None, None, None),
                List(Identifier("p_value")(None, None, None)),
                BlockStatement(List(
                  ExpressionStatement(
                    CallExpression(Super()(None, None, None), List())(None, None, None)
                  )(None, None, None),
                  ExpressionStatement(AssignmentExpression(
                    "=", MemberExpression(ThisExpression()(None, None, None), Identifier("value")(None, None, None))(None, None, None), Identifier("p_value")(None, None, None)
                  )(None, None, None))(None, None, None),
                  ExpressionStatement(AssignmentExpression(
                    "=", MemberExpression(Super()(None, None, None), Identifier("type")(None, None, None))(None, None, None), StringLiteral("some")(None, None, None)
                  )(None, None, None))(None, None, None)
                ))(None, None, None)
              )(None, None, None)
            ))(None, None, None),
          )(None, None, None),

          ClassDeclaration(
            Identifier("none")(None, None, None), Some(Identifier("option")(None, None, None)),
            ClassBody(List(
              ClassMethod(
                ClassMethodKind.Constructor, Identifier("constructor")(None, None, None),
                List(),
                BlockStatement(List(
                  ExpressionStatement(
                    CallExpression(Super()(None, None, None), List())(None, None, None)
                  )(None, None, None),
                  ExpressionStatement(AssignmentExpression(
                    "=", MemberExpression(Super()(None, None, None), Identifier("type")(None, None, None))(None, None, None), StringLiteral("none")(None, None, None)
                  )(None, None, None))(None, None, None)
                ))(None, None, None)
              )(None, None, None)
            ))(None, None, None),
          )(None, None, None),

          FunctionDeclaration(
            Some(Identifier("get_or_else")(None, None, None)),
            List(Identifier("optional")(None, None, None), Identifier("default_value")(None, None, None)),
            BlockStatement(List(
              IfStatement(
                BinaryExpression(
                  BinaryOperator.StrictEqual,
                  StringLiteral("some")(None, None, None),
                  MemberExpression(Identifier("optional")(None, None, None), Identifier("type")(None, None, None))(None, None, None)
                )(None, None, None),
                ReturnStatement(Some(MemberExpression(Identifier("optional")(None, None, None), Identifier("value")(None, None, None))(None, None, None)))(None, None, None),
                Some(ReturnStatement(Some(Identifier("default_value")(None, None, None)))(None, None, None))
              )(None, None, None)
            ))(None, None, None)
          )(None, None, None),

          VariableDeclaration(
            VariableDeclarationKind.Const,
            List(
              VariableDeclarator(
                Identifier("foo")(None, None, None),
                Some(NewExpression(Identifier("some")(None, None, None), List(StringLiteral("foo")(None, None, None)))(None, None, None))
              )(None, None, None),
              VariableDeclarator(
                Identifier("bar")(None, None, None),
                Some(NewExpression(Identifier("none")(None, None, None), List())(None, None, None))
              )(None, None, None)
            )
          )(None, None, None),

          ExpressionStatement(
            CallExpression(
              MemberExpression(Identifier("console")(None, None, None), Identifier("log")(None, None, None))(None, None, None),
              List(CallExpression(
                Identifier("get_or_else")(None, None, None),
                List(Identifier("foo")(None, None, None), NumericLiteral(42)(None, None, None))
              )(None, None, None))
            )(None, None, None)
          )(None, None, None),
          ExpressionStatement(
            CallExpression(
              MemberExpression(Identifier("console")(None, None, None), Identifier("log")(None, None, None))(None, None, None),
              List(CallExpression(
                Identifier("get_or_else")(None, None, None),
                List(Identifier("bar")(None, None, None), NumericLiteral(42)(None, None, None))
              )(None, None, None))
            )(None, None, None)
          )(None, None, None),
        ),
        SourceType.Script, "foo.js"
      )(None, None, None)
    )(None, None, None))(0)
    assertEquals(generator.get.code,
"""class option {
  type;
}
class some extends option {
  value;
  constructor(p_value) {
    super();
    this.value = p_value;
    super.type = "some";
  }
}
class none extends option {
  constructor() {
    super();
    super.type = "none";
  }
}
function get_or_else(optional, default_value) {
  if ("some" === optional.type) {
    return optional.value;
  } else return default_value;
}
const foo = new some("foo"),
  bar = new none()
console.log(get_or_else(foo, 42));
console.log(get_or_else(bar, 42));""")
  }
