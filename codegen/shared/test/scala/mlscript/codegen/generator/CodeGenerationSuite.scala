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
    val printer = Printer(sourceMap)
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.generate(printer.print(Import()(None, None, None))(0))
      assertEquals(generator.get.code, "import")
    }
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.generate(printer.print(EmptyStatement()(None, None, None))(0))
      assertEquals(generator.get.code, ";")
    }
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.generate(printer.print(StringLiteral("abc")(None, None, None))(0))
      assertEquals(generator.get.code, "\"abc\"")
    }
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.generate(printer.print(NumericLiteral(114.514)(None, None, None))(0))
      assertEquals(generator.get.code, "114.514")
    }
  }

  test("Composed Test") {
    val printer = Printer(sourceMap)
    {
      val generator = CodeGenerator(format, sourceMap)
      generator.generate(printer.print(ImportSpecifier(Some(Identifier("bar")(None, None, None)), Identifier("foo")(None, None, None), ImportKind.Value)(None, None, None))(0))
      assertEquals(generator.get.code, "foo as bar")
    }
  }
