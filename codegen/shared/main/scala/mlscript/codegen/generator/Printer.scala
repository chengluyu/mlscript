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
    case ImportSpecifier(local, imported, kind) =>
      val printImported = kind match {
        case ImportKind.Type => print(imported, Some(node), previous ::: List(Word("type"), Space()))
        case ImportKind.TypeOf => print(imported, Some(node), previous ::: List(Word("type"), Space()))
        case _ => print(imported, Some(node), previous)
      }
      local match { // TODO: add location information
        case Some(Identifier(localName, typeAnnotation)) => imported match {
          case Identifier(name, _) if (!name.equals(localName)) =>
            print(Identifier(localName, typeAnnotation)(None, None, None), Some(node), printImported ::: List(Space(), Word("as"), Space()))
          case StringLiteral(str) if (!str.equals(localName)) =>
            print(Identifier(localName, typeAnnotation)(None, None, None), Some(node), printImported ::: List(Space(), Word("as"), Space()))
          case _ => printImported
        }
        case _ => printImported
      }
    case ImportDefaultSpecifier(local) =>
      print(local, Some(node), previous)
    case ExportDefaultSpecifier(exported) =>
      print(exported, Some(node), previous)
    case ExportSpecifier(local, exported, kind) =>
      val printExported = kind match {
        case ExportKind.Type => print(exported, Some(node), previous ::: List(Word("type"), Space()))
        case _ => print(exported, Some(node), previous)
      }
      local match { // TODO: add location information
        case Some(Identifier(localName, typeAnnotation)) => exported match {
          case Identifier(name, _) if (!name.equals(localName)) =>
            print(Identifier(localName, typeAnnotation)(None, None, None), Some(node), printExported ::: List(Space(), Word("as"), Space()))
          case StringLiteral(str) if (!str.equals(localName)) =>
            print(Identifier(localName, typeAnnotation)(None, None, None), Some(node), printExported ::: List(Space(), Word("as"), Space()))
          case _ => printExported
        }
        case _ => printExported
      }
    case ExportNamespaceSpecifier(exported) =>
      print(exported, Some(node), previous ::: List(Token("*"), Space(), Word("as"), Space()))
    case ExportDefaultDeclaration(dec) =>
      print(dec, Some(node), previous ::: List(Word("export"), Space(), Word("default"), Space())) ::: (dec match {
        case s: Statement => List()
        case _ => List(Semicolon())
      })
    case ImportAttribute(key, value) =>
      val printKey = print(key, Some(node), previous)
      print(value, Some(node), printKey ::: List(Token(":"), Space()))
    case ImportNamespaceSpecifier(local) =>
      print(local, Some(node), previous ::: List(Token("*"), Space(), Word("as"), Space()))
    // END modules.scala
    // ---
    // BEGIN statements.scala
    case WithStatement(obj, body) =>
      val printObj = print(obj, Some(node), previous ::: List(Word("with"), Space(), Token("("))) :+ Token(")")
      printBlock(body, Some(node), printObj)
    case ForStatement(init, test, update, body) =>
      val printInit = init match {
        case Some(init) => print(init, Some(node), previous ::: List(Word("for"), Space(), Token("("))) :+ Token(";")
        case _ => List(Word("for"), Space(), Token("("), Token(";"))
      }
      val printTest = test match {
        case Some(test) => print(test, Some(node), printInit :+ Space()) :+ Token(";")
        case _ => printInit :+ Token(";")
      }
      val printUpdate = update match {
        case Some(update) => print(update, Some(node), printTest :+ Space())
        case _ => printTest
      }
      printBlock(body, Some(node), printUpdate :+ Token(")"))
    case WhileStatement(test, body) =>
      val printTest = print(test, Some(node), previous ::: List(Word("while"), Space(), Token("("))) :+ Token(")")
      printBlock(body, Some(node), printTest)
    case ForInStatement(left, right, body) =>
      val printLeft = print(left, Some(node), previous ::: List(Word("for"), Space(), Token("(")))
      val printRight = print(right, Some(node), printLeft ::: List(Word("in"), Space())) :+ Token(")")
      printBlock(body, Some(node), printRight)
    case ForOfStatement(left, right, body, await) =>
      val prefix =
        if (await) List(Word("for"), Space(), Word("await"), Space(), Token("("))
        else List(Word("for"), Space(), Token("("))
      val printLeft = print(left, Some(node), previous ::: prefix)
      val printRight = print(right, Some(node), printLeft ::: List(Word("of"), Space())) :+ Token(")")
      printBlock(body, Some(node), printRight)
    case DoWhileStatement(test, body) =>
      val printBody = print(body, Some(node), previous ::: List(Word("do"), Space()))
      print(test, Some(node), printBody ::: List(Space(), Word("while"), Space(), Token("("))) ::: List(Token(")"), Semicolon())
    case LabeledStatement(label, body) =>
      val printLabel = print(label, Some(node), previous) ::: List(Token(":"), Space())
      print(body, Some(node), printLabel)
    case TryStatement(block, handler, finalizer) =>
      val printBlock = print(block, Some(node), previous ::: List(Word("try"), Space())) :+ Space()
      val printHandler = handler match {
        case Some(handler) => print(handler, Some(node), printBlock)
        case _ => printBlock
      }
      finalizer match {
        case Some(finalizer) => print(finalizer, Some(node), printHandler ::: List(Space(), Word("finally"), Space()))
        case _ => printHandler
      }
    case CatchClause(param, body) =>
      val prefix = List(Word("catch"), Space())
      val printParam = param match {
        case Some(id: Identifier) => id.typeAnnotation match {
          case Some(typeAnnotation) =>
            print(typeAnnotation, Some(node), print(id, Some(node), (previous ::: prefix) :+ Token("("))) ::: List(Token(")"), Space())
          case _ =>
            print(id, Some(node), (previous ::: prefix) :+ Token("(")) ::: List(Token(")"), Space())
        }
        case Some(pat: ArrayPattern) => pat.typeAnnotation match {
          case Some(typeAnnotation) =>
            print(typeAnnotation, Some(node), print(pat, Some(node), (previous ::: prefix) :+ Token("("))) ::: List(Token(")"), Space())
          case _ =>
            print(pat, Some(node), (previous ::: prefix) :+ Token("(")) ::: List(Token(")"), Space())
        }
        case Some(pat: ObjectPattern) => pat.typeAnnotation match {
          case Some(typeAnnotation) =>
            print(typeAnnotation, Some(node), print(pat, Some(node), (previous ::: prefix) :+ Token("("))) ::: List(Token(")"), Space())
          case _ =>
            print(pat, Some(node), (previous ::: prefix) :+ Token("(")) ::: List(Token(")"), Space())
        }
        case _ => prefix
      }
      print(body, Some(node), printParam)
    case DebuggerStatement() =>
      previous ::: List(Word("debugger"), Semicolon())
    case VariableDeclarator(id, init, definite) =>
      val printId = print(id, Some(node), previous) ::: (if (definite) List(Token("!")) else List())
      val printAnnotation = id match {
        case Identifier(_, Some(typeAnnotation)) => print(typeAnnotation, Some(node), printId)
        case RestElement(_, Some(typeAnnotation)) => print(typeAnnotation, Some(node), printId)
        case ArrayPattern(_, Some(typeAnnotation)) => print(typeAnnotation, Some(node), printId)
        case ObjectPattern(_, Some(typeAnnotation)) => print(typeAnnotation, Some(node), printId)
        case TSAsExpression(_, typeAnnotation) => print(typeAnnotation, Some(node), printId)
        case TSSatisfiesExpression(_, typeAnnotation) => print(typeAnnotation, Some(node), printId)
        case TSTypeAssertion(typeAnnotation, _) => print(typeAnnotation, Some(node), printId)
        case _ => printId
      }
      init match {
        case Some(init) => print(init, Some(node), printAnnotation ::: List(Space(), Token("="), Space()))
        case _ => printAnnotation
      }
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
    case Identifier(name, _) =>
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

  private def printBlock(
    body: Node,
    node: Option[Node] = None,
    previous: List[PrintCommand] = List(),
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): List[PrintCommand] = body match {
    case _: EmptyStatement => print(body, node, previous)
    case _ => print(body, node, previous :+ Space())
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
}
