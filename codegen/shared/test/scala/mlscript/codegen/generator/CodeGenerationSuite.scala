package mlscript.codegen.generator

import mlscript.codegen.ast._

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
