package mlscript.codegen.generator

import scala.util.matching.Regex
import scala.collection.mutable.{ArrayBuffer, HashSet}
import mlscript.codegen.{Position, Location, LocationType}
import mlscript.codegen.ast._
import mlscript.codegen.generator._

class Printer(map: SourceMapBuilder) {
  def print(
    node: Node,
    parent: Option[Node] = None,
    previous: List[PrintCommand] = List(),
    stack: List[Node] = List(),
    forceParens: Boolean = false
  )(implicit indentLevel: Int) =
    val shouldPrintParens =
      if (forceParens) true
      else Parentheses.needsParens(node, parent, stack)
    if (shouldPrintParens) 
      translate(node, parent, previous :+ Token("(", false), stack :+ node, forceParens) :+ Token(")", false)
    else
      translate(node, parent, previous, stack :+ node, forceParens)

  private def translate(
    node: Node,
    parent: Option[Node] = None,
    previous: List[PrintCommand] = List(),
    stack: List[Node] = List(),
    forceParens: Boolean = false
  )(implicit indentLevel: Int) = node match {
    // BEGIN base.scala

    // END base.scala
    // ---
    // BEGIN classes.scala

    // END classes.scala
    // ---
    // BEGIN expressions.scala
    case ThisExpression() =>
      previous :+ Word("this")
    case Super() =>
      previous :+ Word("super")
    case Import() =>
      previous :+ Word("import")
    case EmptyStatement() =>
      previous :+ Semicolon(true)
    // END expressions.scala
    // ---
    // BEGIN methods.scala

    // END methods.scala
    // ---
    // BEGIN modules.scala

    // END modules.scala
    // ---
    // BEGIN statements.scala

    // END statements.scala
    // ---
    // BEGIN templates.scala
    case te @ TemplateElement(value, _) =>
      parent match {
        case Some(TemplateLiteral(quasi, _)) =>
          val isFirst = !quasi.isEmpty && quasi.head.equals(te)
          val isLast = !quasi.isEmpty && quasi.last.equals(te)
          previous :+ Token(s"${if (isFirst) "`" else "}"}${value}${if (isLast) "`" else "${"}", true)
        case _ => throw new Exception("wong parent of TemplateElement.")
      }
    // END templates.scala
    // ---
    // BEGIN types.scala
    case Identifier(name) =>
      previous :+ Word(name)
    case ArgumentPlaceholder() =>
      previous :+ Token("?", false)
    case RegExpLiteral(pattern, flags) =>
      previous :+ Word(s"/$$$pattern/$$$flags")
    case BooleanLiteral(value) =>
      previous :+ (if (value) Word("true") else Word("false"))
    case NullLiteral() =>
      previous :+ Word("null")
    case NumericLiteral(value) =>
      previous :+ Number(value.toString())
    case StringLiteral(value) =>
      previous :+ Token(s"\"${value}\"", false)
    case BigIntLiteral(value) =>
      previous :+ Word(s"${value}n")
    case DecimalLiteral(value) =>
      previous :+ Word(s"${value}m")
    case TopicReference() =>
      previous :+ Token("#", false) // TODO: add settings
    case PipelinePrimaryTopicReference() =>
      previous :+ Token("#", false)
    // END types.scala
    // ---
    // BEGIN typescript.scala
    case TSAnyKeyword() => previous :+ Word("any")
    case TSBigIntKeyword() => previous :+ Word("bigint")
    case TSUnknownKeyword() => previous :+ Word("unknown")
    case TSNumberKeyword() => previous :+ Word("number")
    case TSObjectKeyword() => previous :+ Word("object")
    case TSBooleanKeyword() => previous :+ Word("boolean")
    case TSStringKeyword() => previous :+ Word("string")
    case TSSymbolKeyword() => previous :+ Word("symbol")
    case TSVoidKeyword() => previous :+ Word("void")
    case TSUndefinedKeyword() => previous :+ Word("undefined")
    case TSNullKeyword() => previous :+ Word("null")
    case TSNeverKeyword() => previous :+ Word("never")
    case TSIntrinsicKeyword() => previous :+ Word("intrinsic")
    case TSThisType() => previous :+ Word("this")
    // END typescript.scala
  }
}

object Printer {
  def apply(map: SourceMapBuilder) = new Printer(map)
}
