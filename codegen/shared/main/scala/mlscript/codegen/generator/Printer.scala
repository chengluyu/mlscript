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
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): List[PrintCommand] =
    val shouldPrintParens =
      if (forceParens) true
      else Parentheses.needsParens(node, parent, stack)
    if (shouldPrintParens) 
      translate(node, parent, previous :+ Token("("), forceParens)(indentLevel, stack :+ node) :+ Token(")")
    else
      translate(node, parent, previous, forceParens)(indentLevel, stack :+ node)

  import Printer._

  private def translate(
    node: Node,
    parent: Option[Node] = None,
    previous: List[PrintCommand] = List(),
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): List[PrintCommand] = node match {
    // BEGIN base.scala
    case File(program) => print(program, Some(node), previous)
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
    case DoExpression(body, async) =>
      val lst =
        if (async) List(Word("async"), Space(), Word("do"), Space())
        else List(Word("do"), Space())
      print(body, Some(node), previous ::: lst)
    case ParenthesizedExpression(exp) =>
      print(exp, Some(node), previous :+ Token("(")) :+ Token(")")
    case ConditionalExpression(test, consequent, alternate) =>
      val printTest = print(test, Some(node), previous)
      val printConseq = print(consequent, Some(node), printTest ::: List(Space(), Token("?"), Space()))
      print(alternate, Some(node), printConseq ::: List(Space(), Token(":"), Space()))
    case Decorator(exp) =>
      val prefix =
        if (shouldParenthesizeDecoratorExpression(exp)) List(Token("@"), Token("("))
        else List(Token("@"))
      val suffix =
        if (shouldParenthesizeDecoratorExpression(exp)) List(Token(")"), Newline())
        else List(Newline())
      print(exp, Some(node), previous ::: prefix) ::: suffix
    case OptionalMemberExpression(obj, prop, computed, optional) =>
      val printObj = print(obj, Some(node), previous)
      (computed, optional) match {
        case (true, true) => print(prop, Some(node), previous ::: List(Token("?."), Token("["))) :+ Token("]")
        case (true, false) => print(prop, Some(node), previous :+ Token("[")) :+ Token("]")
        case (false, true) => print(prop, Some(node), previous :+ Token("?."))
        case _ => print(prop, Some(node), previous :+ Token("."))
      }
    case ExpressionStatement(exp) =>
      print(exp, Some(node), previous) :+ Semicolon()
    case AssignmentPattern(left, right) =>
      val printLeft = print(left, Some(node), previous)
      print(right, Some(node), printLeft ::: List(Space(), Token("="), Space()))
    case AssignmentExpression(op, left, right) => // TODO: check if parens are necessary
      val printLeft = print(left, Some(node), previous :+ Token("(")) :+ Space()
      val opPrint = if (op.equals("in") || op.equals("instanceof")) Word(op) else Token(op)
      print(right, Some(node), printLeft ::: List(opPrint, Space())) :+ Token(")")
    case BindExpression(obj, callee) =>
      val printObj = print(obj, Some(node), previous)
      print(callee, Some(node), printObj :+ Token("::"))
    case BinaryExpression(op, left, right) =>
      val printLeft = print(left, Some(node), previous :+ Token("(")) :+ Space()
      val ops = BinaryOperator.to(op)
      val opPrint = if (ops.equals("in") || ops.equals("instanceof")) Word(ops) else Token(ops)
      print(right, Some(node), printLeft ::: List(opPrint, Space())) :+ Token(")")
    case LogicalExpression(op, left, right) =>
      val printLeft = print(left, Some(node), previous :+ Token("(")) :+ Space()
      val ops = LogicalOperator.to(op)
      val opPrint = if (ops.equals("in") || ops.equals("instanceof")) Word(ops) else Token(ops)
      print(right, Some(node), printLeft ::: List(opPrint, Space())) :+ Token(")")
    case MemberExpression(obj, prop, computed) =>
      val printObj = print(obj, Some(node), previous)
      if (computed) print(prop, Some(node), printObj :+ Token("[")) :+ Token("]")
      else print(prop, Some(node), printObj :+ Token("."))
    case MetaProperty(meta, prop) =>
      val printMeta = print(meta, Some(node), previous)
      print(prop, Some(node), printMeta :+ Token("."))
    case PrivateName(id) =>
      print(id, Some(node), previous :+ Token("#"))
    case V8IntrinsicIdentifier(name) =>
      previous ::: List(Token("%"), Word(name))
    case ModuleExpression(body) =>
      val prefix = List(Word("module"), Space(), Token("{"), Newline()(indentLevel + 1))
      print(body, Some(node), previous ::: prefix)(indentLevel + 1, stack) :+ Token("}")
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
      previous :+ Token("?")
    case RegExpLiteral(pattern, flags) =>
      previous :+ Word(s"/$$$pattern/$$$flags")
    case BooleanLiteral(value) =>
      previous :+ (if (value) Word("true") else Word("false"))
    case NullLiteral() =>
      previous :+ Word("null")
    case NumericLiteral(value) =>
      previous :+ Number(value.toString())
    case StringLiteral(value) =>
      previous :+ Token(s"\"${value}\"")
    case BigIntLiteral(value) =>
      previous :+ Word(s"${value}n")
    case DecimalLiteral(value) =>
      previous :+ Word(s"${value}m")
    case TopicReference() =>
      previous :+ Token("#") // TODO: add settings
    case PipelinePrimaryTopicReference() =>
      previous :+ Token("#")
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

  private def isDecoratorMemberExpression(node: Node): Boolean =
    node match {
      case i: Identifier => true
      case MemberExpression(obj, prop, computed) =>
        !computed && (prop match {
          case i: Identifier => true
          case _ => false
        }) && isDecoratorMemberExpression(obj)
      case _ => false
    }

  private def shouldParenthesizeDecoratorExpression(node: Node): Boolean =
    node match {
      case p: ParenthesizedExpression => false
      case e: CallExpression => !isDecoratorMemberExpression(e.callee)
      case _ => !isDecoratorMemberExpression(node)
    }

  // private def parameters(parameter: List[Identifier | RestElement | Node with Pattern | TSParameterProperty], parent: Node) =
  //   parameter.iterator.zipWithIndex.foreach((p, i) => {
  //     param(p, Some(parent))
  //     if (i < parameter.length - 1) token(","); space()
  //   })

  // private def predicate(
  //   node: FunctionDeclaration | FunctionExpression | ArrowFunctionExpression,
  //   noLineTerminatorAfter: Boolean = false
  // ) = {
  //   val pred = node match {
  //     case d: FunctionDeclaration => d.predicate
  //     case e: FunctionExpression => e.predicate
  //     case e: ArrowFunctionExpression => e.predicate
  //   }
  //   val rt = node match {
  //     case d: FunctionDeclaration => d.returnType
  //     case e: FunctionExpression => e.returnType
  //     case e: ArrowFunctionExpression => e.returnType
  //   }

  //   pred match {
  //     case Some(_) =>
  //       if (rt.isEmpty) token(":")
  //       space(); print(pred, Some(node), noLineTerminatorAfter)
  //     case _ => ()
  //   }
  // }

  // private def functionHead(
  //   node: FunctionDeclaration | FunctionExpression | TSDeclareFunction
  // )(implicit options: PrinterOptions) = {
  //   val async = node match {
  //     case FunctionDeclaration(_, _, _, _, async) => async
  //     case FunctionExpression(_, _, _, _, async) => async
  //     case d: TSDeclareFunction => d.async.getOrElse(false)
  //   }

  //   if (async) {
  //     word("async")
  //     space()
  //   }

  //   word("function")

  //   val generator = node match {
  //     case FunctionDeclaration(_, _, _, generator, _) => generator
  //     case FunctionExpression(_, _, _, generator, _) => generator
  //     case d: TSDeclareFunction => d.generator.getOrElse(false)
  //   }

  //   if (generator) {
  //     token("*")
  //   }

  //   space()
  //   val id = node match {
  //     case FunctionDeclaration(id, _, _, _, _) => id
  //     case FunctionExpression(id, _, _, _, _) => id
  //     case TSDeclareFunction(id, _, _, _) => id
  //   }
    
  //   id match {
  //     case Some(id) => print(id, Some(node))
  //     case _ => ()
  //   }

  //   val tp = node match {
  //     case d: FunctionDeclaration => d.typeParameters
  //     case e: FunctionExpression => e.typeParameters
  //     case d: TSDeclareFunction => d.typeParameters
  //   }
  //   print(tp, Some(node))

  //   val params = node match {
  //     case FunctionDeclaration(_, p, _, _, _) => p
  //     case FunctionExpression(_, p, _, _, _) => p
  //     case TSDeclareFunction(_, _, p, _) => p
  //   }
  //   token("(")
  //   parameters(params, node)
  //   token(")")

  //   val rp = node match {
  //     case d: FunctionDeclaration => d.returnType
  //     case e: FunctionExpression => e.returnType
  //     case d: TSDeclareFunction => d.returnType
  //   }
  //   print(rp, Some(node), false)

  //   node match {
  //     case d: FunctionDeclaration => predicate(d)
  //     case e: FunctionExpression => predicate(e)
  //     case _ => ()
  //   }
  // }
}
