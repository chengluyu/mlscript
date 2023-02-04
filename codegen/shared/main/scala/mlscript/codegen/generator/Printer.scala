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
    case Program(body, _, _) => printJoin(body, Some(node), previous, statement = true)
    case BlockStatement(body) =>
      printJoin(body, Some(node), previous :+ Token("{"), statement = true, indent = true) :+ Token("}")
    // END base.scala
    // ---
    // BEGIN classes.scala
    case ClassDeclaration(id, superClass, body, decorators, abs, dec, implements, superTypeParameters, typeParameters) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printDeclare = if (dec) List(Word("declare"), Space()) else List()
      val printAbs = if (abs) List(Word("abstract"), Space()) else List()
      val printID = print(id, Some(node), (printDecoractors ::: printDeclare ::: printAbs) :+ Word("class"))
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printID)
        case _ => printID
      }
      val printSuper = (superClass, superTypeParameters) match {
        case (Some(superClass), Some(superTypeParameters)) =>
          print(superTypeParameters, Some(node), print(superClass, Some(node), printType ::: List(Space(), Word("extends"), Space())))
        case (Some(superClass), None) =>
          print(superClass, Some(node), printType ::: List(Space(), Word("extends"), Space()))
        case _ => printType
      }
      val printImplement = implements match {
        case Some(implements) =>
          printJoin(implements, Some(node), printSuper ::: List(Space(), Word("implements"), Space()), statement = true)
        case _ => printSuper
      }
      print(body, Some(node), printImplement :+ Space())
    case ClassExpression(id, superClass, body, decorators, implements, superTypeParameters, typeParameters) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printID = print(id, Some(node), printDecoractors :+ Word("class"))
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printID)
        case _ => printID
      }
      val printSuper = (superClass, superTypeParameters) match {
        case (Some(superClass), Some(superTypeParameters)) =>
          print(superTypeParameters, Some(node), print(superClass, Some(node), printType ::: List(Space(), Word("extends"), Space())))
        case (Some(superClass), None) =>
          print(superClass, Some(node), printType ::: List(Space(), Word("extends"), Space()))
        case _ => printType
      }
      val printImplement = implements match {
        case Some(implements) =>
          printJoin(implements, Some(node), printSuper ::: List(Space(), Word("implements"), Space()), statement = true)
        case _ => printSuper
      }
      print(body, Some(node), printImplement :+ Space())
    case ClassBody(body) =>
      if (body.length == 0) previous ::: List(Token("{"), Token("}"))
      else 
        val res = printJoin(body, Some(node), previous ::: List(Token("{"), Newline()))(indentLevel + 1, stack)
        res.last match {
          case _: Newline => res :+ Token("}")
          case _ => res ::: List(Newline(), Token("}"))
        }
    case ClassProperty(key, value, decorators, typeAnnotation, computed, static, abs, access, declare, definite, optional, overrided, readonly) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printModifier = tsPrintClassMemberModifiers(true, declare, access, static, overrided, abs, readonly)
      val printKey =
        if (computed) print(key, Some(node), (printDecoractors ::: printModifier) :+ Token("[")) :+ Token("]")
        else print(key, Some(node), printDecoractors ::: printModifier)
      val printOptional = if (optional) printKey :+ Token("?") else printKey
      val printDefinite = if (definite) printOptional :+ Token("!") else printOptional
      val printType = typeAnnotation match {
        case Some(typeAnnotation) =>
          print(typeAnnotation, Some(node), printDefinite)
        case _ => printDefinite
      }
      (value match {
        case Some(value) =>
          print(value, Some(node), printType ::: List(Space(), Token("="), Space()))
        case _ => printType
      }) :+ Semicolon()
    case ClassAccessorProperty(key, value, decorators, typeAnnotation, computed, static, abs, access, declare, definite, optional, overrided, readonly) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printModifier =
        tsPrintClassMemberModifiers(true, declare, access, static, overrided, abs, readonly) ::: List(Word("accessor"), Space())
      val printKey =
        if (computed) print(key, Some(node), (printDecoractors ::: printModifier) :+ Token("[")) :+ Token("]")
        else print(key, Some(node), printDecoractors ::: printModifier)
      val printOptional = if (optional) printKey :+ Token("?") else printKey
      val printDefinite = if (definite) printOptional :+ Token("!") else printOptional
      val printType = typeAnnotation match {
        case Some(typeAnnotation) =>
          print(typeAnnotation, Some(node), printDefinite)
        case _ => printDefinite
      }
      (value match {
        case Some(value) =>
          print(value, Some(node), printType ::: List(Space(), Token("="), Space()))
        case _ => printType
      }) :+ Semicolon()
    case ClassPrivateProperty(key, value, decorators, typeAnnotation, static, definite, readonly) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printStatic = if (static) List(Word("static"), Space()) else List()
      val printKey = print(key, Some(node), printDecoractors ::: printStatic)
      val printType = typeAnnotation match {
        case Some(typeAnnotation) =>
          print(typeAnnotation, Some(node), printKey)
        case _ => printKey
      }
      (value match {
        case Some(value) =>
          print(value, Some(node), printType ::: List(Space(), Token("="), Space()))
        case _ => printType
      }) :+ Semicolon()
    case method: ClassMethod =>
      print(method.body, Some(node), classMethodHead(method, previous) :+ Space())
    case method: ClassPrivateMethod =>
      print(method.body, Some(node), classMethodHead(method, previous) :+ Space())
    case StaticBlock(body) =>
      if (body.length == 0) previous ::: List(Word("static"), Space(), Token("{"), Token("}"))
      else
        printJoin(body, Some(node), previous ::: List(Word("static"), Space(), Token("{"), Newline()), indent = true) :+ Token("}")
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
    case NewExpression(callee, arguments, _, typeParameters) =>
      val printCallee = print(callee, Some(node), previous ::: List(Word("new"), Space()))
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printCallee)
        case _ => printCallee
      }
      printJoin(arguments, Some(node), printType :+ Token("("), List(Token(","), Space())) :+ Token(")")
    case SequenceExpression(expressions) =>
      printJoin(expressions, Some(node), previous, List(Token(","), Space()))
    case OptionalCallExpression(callee, arguments, optional, typeParameters) =>
      val printCallee = print(callee, Some(node), previous)
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printCallee)
        case _ => printCallee
      }
      val printOptional = if (optional) printType :+ Token("?.") else printType
      printJoin(arguments, Some(node), printOptional :+ Token("("), List(Token(","), Space())) :+ Token(")")
    case CallExpression(callee, arguments, optional, typeParameters) =>
      val printCallee = print(callee, Some(node), previous)
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printCallee)
        case _ => printCallee
      }
      printJoin(arguments, Some(node), printType :+ Token("("), List(Token(","), Space())) :+ Token(")")
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
        case RestElement(_, Some(typeAnnotation), _, _) => print(typeAnnotation, Some(node), printId)
        case ArrayPattern(_, Some(typeAnnotation), _, _) => print(typeAnnotation, Some(node), printId)
        case ObjectPattern(_, Some(typeAnnotation), _) => print(typeAnnotation, Some(node), printId)
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
    case TaggedTemplateExpression(tag, quasi, typeParameters) =>
      val printTag = print(tag, Some(node), previous)
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printTag)
        case _ => printTag
      }
      print(quasi, Some(node), printType)
    case TemplateLiteral(quasis, exprs) =>
      quasis.iterator.zipWithIndex.foldLeft(previous)((prev, pair) =>
        if (pair._2 + 1 < quasis.length) print(exprs(pair._2), Some(node), print(pair._1, Some(node), prev))
        else print(pair._1, Some(node), prev))
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
    case RestElement(arg, _, _, _) =>
      print(arg, Some(node), previous :+ Token("..."))
    case ArrayExpression(elements) =>
      elements.iterator.zipWithIndex.foldLeft(previous :+ Token("["))((prev, pair) => pair match {
        case (Some(ele), i) =>
          print(ele, Some(node), if (i > 0) prev :+ Space() else prev) ::: (if (i < elements.length) List(Token(",")) else List())
        case _ => prev :+ Token(",")
      }) :+ Token("]")
    case TupleExpression(elements) => // TODO: add settings
      elements.iterator.zipWithIndex.foldLeft(previous :+ Token("#["))((prev, pair) => pair match {
        case (ele, i) =>
          print(ele, Some(node), if (i > 0) prev :+ Space() else prev) ::: (if (i < elements.length) List(Token(",")) else List())
      }) :+ Token("]")
    case PipelineTopicExpression(expression) =>
      print(expression, Some(node), previous)
    case PipelineBareFunction(callee) =>
      print(callee, Some(node), previous)
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
    case TSTypeAnnotation(typeAnnotation) =>
      print(typeAnnotation, Some(node), previous ::: List(Token(":"), Space()))
    case TSTypeParameter(constraint, default, name, in, out) =>
      val printIn = if (in) List(Word("in"), Space()) else List()
      val printOut = if (out) List(Word("out"), Space()) else List()
      val printConstraint = constraint match {
        case Some(constraint) =>
          print(constraint, Some(node), previous ::: printIn ::: printOut ::: List(Word(name), Space(), Word("extends"), Space()))
        case _ => (previous ::: printIn ::: printOut) :+ Word(name)
      }
      default match {
        case Some(default) =>
          print(default, Some(node), printConstraint ::: List(Space(), Token("="), Space()))
        case _ => printConstraint
      }
    case TSQualifiedName(left, right) =>
      print(right, Some(node), print(left, Some(node), previous) :+ Token("."))
    case sig @ TSPropertySignature(key, typeAnnotation, init, _, _, _, readonly) =>
      val prefix = if (readonly) List(Word("readonly"), Space()) else List()
      val printProperty = tsPrintPropertyOrMethodName(sig, parent, previous ::: prefix)
      val printType = typeAnnotation match {
        case Some(typeAnnotation) => print(typeAnnotation, Some(node), printProperty)
        case _ => printProperty
      }
      (init match {
        case Some(init) => print(init, Some(node), printType ::: List(Space(), Token("="), Space()))
        case _ => printType
      }) :+ Token(";")
    case TSTypeReference(typeName, typeParameter) => typeParameter match {
      case Some(typeParameter) =>
        print(typeParameter, Some(node), print(typeName, Some(node), previous, true), true)
      case _ => print(typeName, Some(node), previous, true)
    }
    case TSTypePredicate(parameterName, typeAnnotation, asserts) =>
      val prefix = if (asserts) List(Word("asserts"), Space()) else List()
      val printName = print(parameterName, Some(node), previous ::: prefix)
      typeAnnotation match {
        case Some(TSTypeAnnotation(typeAnnotation)) =>
          print(typeAnnotation, None, printName ::: List(Space(), Word("is"), Space()))
        case _ => printName
      }
    case TSTypeQuery(exprName, typeParameter) =>
      val printName = print(exprName, Some(node), previous ::: List(Word("typeof"), Space()))
      typeParameter match {
        case Some(typeParameter) => print(typeParameter, Some(node), printName)
        case _ => printName
      }
    case TSTypeLiteral(members) => tsPrintBraced(members, Some(node), previous)
    case TSArrayType(elementType) => print(elementType, Some(node), previous) :+ Token("[]")
    case TSOptionalType(typeAnnotation) => print(typeAnnotation, Some(node), previous) :+ Token("?")
    case TSRestType(typeAnnotation) => print(typeAnnotation, Some(node), previous :+ Token("..."))
    case TSNamedTupleMember(label, elementType, optional) =>
      val printLabel = print(label, Some(node), previous) ::: (if (optional) List(Token("?")) else List())
      print(elementType, Some(node), printLabel ::: List(Token(":"), Space()))
    case TSConditionalType(checkType, extendsType, trueType, falseType) =>
      print(falseType, Some(node),
        print(trueType, Some(node), 
          print(extendsType, Some(node),
            print(checkType, Some(node), previous) ::: List(Space(), Word("extends"), Space()))
          ::: List(Space(), Token("?"), Space()))
        ::: List(Space(), Token(":"), Space()))
    case TSInferType(typeParameter) =>
      print(typeParameter, Some(node), previous ::: List(Token("infer"), Space()))
    case TSParenthesizedType(typeAnnotation) =>
      print(typeAnnotation, Some(node), previous :+ Token("(")) :+ Token(")")
    case TSTypeOperator(typeAnnotation, operator) =>
      print(typeAnnotation, Some(node), previous ::: List(Word(operator), Space()))
    case TSIndexedAccessType(objectType, indexType) =>
      print(indexType, Some(node), print(objectType, Some(node), previous, true) :+ Token("[")) :+ Token("]")
    case TSMappedType(typeParameter, typeAnnotation, nameType, optional, readonly) =>
      val prefix = List(Token("{"), Space()) :::
        (if (readonly.isDefined) tokenIfPlusMinus(readonly) ::: List(Word("readonly"), Space(), Token("[")) else List(Token("[")))
      val printConstraint = typeParameter.constraint match {
        case Some(constraint) =>
          print(constraint, Some(typeParameter), previous ::: prefix ::: List(Word(typeParameter.name), Space(), Word("in"), Space()))
        case _ => previous ::: prefix ::: List(Word(typeParameter.name), Space(), Word("in"), Space())
      }
      val printName = (nameType match {
        case Some(nameType) => print(nameType, Some(node), printConstraint ::: List(Space(), Word("as"), Space()))
        case _ => printConstraint
      }) :+ Token("]")
      val printOptional = if (optional.isDefined) tokenIfPlusMinus(optional) :+ Token("?") else List()
      (typeAnnotation match {
        case Some(typeAnnotation) =>
          print(typeAnnotation, Some(node), printName ::: printOptional ::: List(Token(":"), Space()))
        case _ => printName ::: printOptional ::: List(Token(":"), Space())
      }) ::: List(Space(), Token("}"))
    case TSLiteralType(literal) => print(literal, Some(node), previous)
    case TSExpressionWithTypeArguments(expr, typeParameters) => typeParameters match {
      case Some(typeParameters) => print(typeParameters, Some(node), print(expr, Some(node), previous))
      case _ => print(expr, Some(node), previous)
    }
    case TSInterfaceBody(body) => tsPrintBraced(body, Some(node), previous)
    case TSTypeAliasDeclaration(id, typeParameter, typeAnnotation, declare) =>
      val prefix = if (declare) List(Word("declare"), Space(), Word("type"), Space()) else List(Word("type"), Space())
      val printID = print(id, Some(node), previous ::: prefix)
      val printType = typeParameter match {
        case Some(typeParameter) => print(typeParameter, Some(node), printID)
        case _ => printID
      }
      print(typeAnnotation, Some(node), printType ::: List(Space(), Token("="), Space())) :+ Token(";")
    case TSAsExpression(expression, typeAnnotation) =>
      print(typeAnnotation, Some(node), print(expression, Some(node), previous) ::: List(Space(), Word("as"), Space()))
    case TSSatisfiesExpression(expression, typeAnnotation) =>
      print(typeAnnotation, Some(node), print(expression, Some(node), previous) ::: List(Space(), Word("satisfies"), Space()))
    case TSTypeAssertion(typeAnnotation, expression) =>
      print(expression, Some(node), print(typeAnnotation, Some(node), previous :+ Token("<")) ::: List(Token(">"), Space()))
    case TSInstantiationExpression(expression, typeParameters) => typeParameters match {
      case Some(typeParameters) =>
        print(typeParameters, Some(node), print(expression, Some(node), previous))
      case _ => print(expression, Some(node), previous)
    }
    case TSEnumDeclaration(id, members, const, declare, _) =>
      val printDeclare = if (declare) List(Word("declare"), Space()) else List()
      val printConst = if (const) List(Word("const"), Space()) else List()
      val printID = print(id, Some(node), previous ::: printDeclare ::: printConst ::: List(Word("enum"), Space()))
      tsPrintBraced(members, Some(node), printID :+ Space())
    case TSEnumMember(id, init) =>
      val printID = print(id, Some(node))
      (init match {
        case Some(init) => print(init, Some(node), printID ::: List(Space(), Token("="), Space()))
        case _ => printID
      }) :+ Token(",")
    case TSModuleDeclaration(id, body, declare, global) =>
      val printDeclare = if (declare) List(Word("declare"), Space()) else List()
      val printGlobal = if (!global) (id match {
        case _: Identifier => List(Word("namespace"), Space())
        case _ => List(Word("module"), Space())
      }) else List()
      val printID = print(id, Some(node), previous ::: printDeclare ::: printGlobal)
      def printBody(body: TSModuleBlock | TSModuleDeclaration, prev: List[PrintCommand]): (List[PrintCommand], TSModuleBlock) = body match {
        case TSModuleDeclaration(id, sub, _, _) => printBody(sub, print(id, Some(body), prev :+ Token(".")))
        case block: TSModuleBlock => (prev, block)
      }
      printBody(body, printID) match {
        case (res, block) => print(block, Some(node), res :+ Space())
      }
    case TSModuleBlock(body) => tsPrintBraced(body, Some(node), previous)
    case TSImportType(argument, qualifier, typeParameters) =>
      val printArgument = print(argument, Some(node), previous ::: List(Word("import"), Token("("))) :+ Token(")")
      val printQualifier = qualifier match {
        case Some(qualifier) => print(qualifier, Some(node), printArgument :+ Token("."))
        case _ => printArgument
      }
      typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printQualifier)
        case _ => printQualifier
      }
    case TSImportEqualsDeclaration(id, module, isExport, _) =>
      val prefix =
        if (isExport) List(Word("export"), Space(), Word("import"), Space())
        else List(Word("import"), Space())
      val printID = print(id, Some(node), previous ::: prefix)
      print(module, Some(node), printID ::: List(Space(), Token("="), Space())) :+ Token(";")
    case TSExternalModuleReference(exp) =>
      print(exp, Some(node), previous :+ Token("require(")) :+ Token(")")
    case TSNonNullExpression(exp) => print(exp, Some(node), previous) :+ Token("!")
    case TSExportAssignment(exp) =>
      print(exp, Some(node), previous ::: List(Word("export"), Space(), Token("="), Space())) :+ Token(";")
    case TSNamespaceExportDeclaration(id) =>
      print(id, Some(node), previous ::: List(Word("export"), Space(), Word("as"), Space(), Word("namespace"), Space()))
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

  private def tsPrintPropertyOrMethodName(
    node: TSPropertySignature | TSMethodSignature,
    parent: Option[Node] = None,
    previous: List[PrintCommand] = List(),
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): List[PrintCommand] = node match {
    case TSPropertySignature(key, _, _, _, computed, optional, _) =>
      print(key, Some(node), previous) :::
        (if (computed) List(Token("]")) else List()) :::
        (if (optional) List(Token("?")) else List())
    case TSMethodSignature(key, _, _, _, _, computed, optional) =>
      print(key, Some(node), previous) :::
        (if (computed) List(Token("]")) else List()) :::
        (if (optional) List(Token("?")) else List())
  }

  private def tsPrintBraced(
    members: List[Node],
    node: Option[Node] = None,
    previous: List[PrintCommand] = List(),
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): List[PrintCommand] =
    members.foldLeft(previous ::: List(Token("{"), Newline()(indentLevel + 1)))(
      (prev, mem) => print(mem, node, prev)(indentLevel + 1, stack) :+ Newline()(indentLevel + 1)) :+ Token("}")

  private def printJoin(
    nodes: List[Node],
    parent: Option[Node],
    previous: List[PrintCommand],
    separator: List[PrintCommand] = List(),
    statement: Boolean = false,
    indent: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node]): List[PrintCommand] = {
    val newIndent = if (indent) indentLevel + 1 else indentLevel
    nodes.iterator.zipWithIndex.foldLeft(previous)((prev, pair) => pair match {
      case (node, i) =>
        val prefix = if (statement && i == 0) List(Newline()(newIndent)) else List()
        val printNode = print(node, parent, prev ::: prefix)(newIndent, stack)
        val printSep = if (i + 1 < nodes.length) printNode ::: indentCommands(separator, newIndent) else printNode
        if (statement) printSep :+ Newline()(newIndent) else printSep
    })
  }

  private def param(
    parameter: Node,
    parent: Option[Node],
    previous: List[PrintCommand],
    decorators: List[Decorator] = List(),
    optional: Boolean = false,
    typeAnnotation: Option[TSTypeAnnotation] = None,
  )(implicit indentLevel: Int, stack: List[Node]): List[PrintCommand] = {
    val printDecoractors = printJoin(decorators, Some(parameter), previous)
    val printParam =
      if (optional) print(parameter, parent, printDecoractors) :+ Token("?")
      else print(parameter, parent, printDecoractors)
    typeAnnotation match {
        case Some(typeAnnotation) =>
          print(typeAnnotation, Some(parameter), printParam)
        case _ => printParam
      }
  }

  private def parameters(
    params: List[Identifier | RestElement | Node with Pattern | TSParameterProperty],
    parent: Option[Node],
    previous: List[PrintCommand] = List(),
  )(implicit indentLevel: Int, stack: List[Node]): List[PrintCommand] =
    params.iterator.zipWithIndex.foldLeft(previous)((prev, pair) => pair match {
      case (p, i) =>
        val suffix = if (i + 1 < params.length) List(Token(","), Space()) else List()
        (p match {
          case id @ Identifier(_, typeAnnotation) =>
            param(id, parent, prev, List(), false, typeAnnotation)
          case rest @ RestElement(_, typeAnnotation, decorators, optional) =>
            param(rest, parent, prev, decorators, optional, typeAnnotation)
          case ap @ ArrayPattern(_, typeAnnotation, decorators, optional) =>
            param(ap, parent, prev, decorators, optional, typeAnnotation)
          case op @ ObjectPattern(_, typeAnnotation, decorators) =>
            param(op, parent, prev, decorators, false, typeAnnotation)
          case pp @ TSParameterProperty(_, _, decorators, _, _) =>
            param(pp, parent, prev, decorators)
          case p: Node with Pattern => param(p, parent, prev)
        }) ::: suffix
    })

  private def params(
    typeParameter: Option[Node],
    params: List[Identifier | RestElement | Node with Pattern | TSParameterProperty],
    returnType: Option[Node],
    parent: Option[Node] = None,
    previous: List[PrintCommand]
  )(implicit indentLevel: Int, stack: List[Node]): List[PrintCommand] =
    (typeParameter, returnType) match {
      case (Some(typeParameter), Some(returnType)) =>
        print(returnType, parent, parameters(params, parent, print(typeParameter, parent, previous) :+ Token("(")) :+ Token(")"))
      case (None, Some(returnType)) =>
        print(returnType, parent, parameters(params, parent, previous :+ Token("(")) :+ Token(")"))
      case (Some(typeParameter), None) =>
        parameters(params, parent, print(typeParameter, parent, previous) :+ Token("(")) :+ Token(")")
      case _ => parameters(params, parent, previous :+ Token("(")) :+ Token(")")
    }
    
  
  private def methodHead(
    typeParameter: Option[Node],
    parameters: List[Identifier | RestElement | Node with Pattern | TSParameterProperty],
    returnType: Option[Node],
    parent: Option[Node],
    previous: List[PrintCommand],
    kind: "get" | "set" | "method" | "init" | "constructor",
    key: Node,
    async: Boolean = false,
    generator: Boolean = false,
    computed: Boolean = false,
    optional: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node]): List[PrintCommand] = {
    val printKind =
      if (kind.equals("get") || kind.equals("set")) List(Word(kind), Space())
      else List()
    val printAsync =
      if (async) List(Word("async"), Space())
      else List()
    val printGenerator =
      if ((kind.equals("method") || kind.equals("init")) && generator) (printKind ::: printAsync) :+ Token("*")
      else printKind ::: printAsync
    val printKey =
      if (computed) print(key, parent, printGenerator :+ Token("[")) :+ Token("]")
      else print(key, parent, printGenerator)
    params(typeParameter, parameters, returnType, parent, if (optional) printKey :+ Token("?") else printKey)
  }

  private def classMethodHead(
    node: ClassMethod | ClassPrivateMethod | TSDeclareMethod,
    previous: List[PrintCommand]
  )(implicit indentLevel: Int, stack: List[Node]): List[PrintCommand] = node match {
    case ClassMethod(kind, key, params, _, computed, static, generator, async, abs, access,
      decorators, optional, overrided, returnType, typeParameter) =>
        methodHead(typeParameter, params, returnType, Some(node),
          printJoin(decorators, Some(node), previous) ::: tsPrintClassMemberModifiers(false, false, access, static, overrided, abs, false),
          kind match {
            case ClassMethodKind.Method => "method"
            case ClassMethodKind.Constructor => "constructor"
            case ClassMethodKind.Getter => "get"
            case ClassMethodKind.Setter => "set"
          },
          key, async, generator, computed, optional
        )
    case ClassPrivateMethod(kind, key, params, _, static, abs, access, async, computed, decorators, generator,
       optional, overrided, returnType, typeParameter) =>
        methodHead(typeParameter, params, returnType, Some(node),
          printJoin(decorators, Some(node), previous) ::: tsPrintClassMemberModifiers(false, false, access, static, overrided, abs, false),
          kind match {
            case ClassPrivateMethodKind.Method => "method"
            case ClassPrivateMethodKind.Getter => "get"
            case ClassPrivateMethodKind.Setter => "set"
          },
          key, async, generator, computed, optional
        )
    case TSDeclareMethod(decorators, key, typeParameter, params, returnType, abs, access,
      async, computed, generator, kind, optional, overrided, static) =>
        methodHead(typeParameter, params, returnType, Some(node),
          printJoin(decorators, Some(node), previous) ::: tsPrintClassMemberModifiers(false, false, access, static, overrided, abs, false),
          kind match {
            case TSDeclareMethodKind.Method => "method"
            case TSDeclareMethodKind.Constructor => "constructor"
            case TSDeclareMethodKind.Getter => "get"
            case TSDeclareMethodKind.Setter => "set"
          },
          key, async, generator, computed, optional
        )
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
  
  private def tokenIfPlusMinus(tok: Option[true | "+" | "-"]) = tok match {
    case Some(true) => List()
    case Some("+") => List(Token("+"))
    case Some("-") => List(Token("-"))
    case _ => List()
  }

  private def indentCommands(cmd: List[PrintCommand], newIndent: Int) =
    cmd.map(pc => pc match {
      case Semicolon(force) => Semicolon(force)(newIndent)
      case Space(force) => Space(force)(newIndent)
      case Word(str) => Word(str)(newIndent)
      case Number(str) => Number(str)(newIndent)
      case Token(str, maybeNewline) => Token(str, maybeNewline)(newIndent)
      case Newline(i, force) => Newline(i, force)(newIndent)
    })

  private def tsPrintClassMemberModifiers(
    isField: Boolean, declare: Boolean, access: Option[AccessModifier], static: Boolean, overrided: Boolean, abs: Boolean, readonly: Boolean
  ) = {
    val printDeclare = if (isField && declare) List(Word("declare"), Space()) else List()
    val printAccess = access match {
      case Some(AccessModifier.Public) => List(Word("public"), Space())
      case Some(AccessModifier.Private) => List(Word("private"), Space())
      case Some(AccessModifier.Protected) => List(Word("protected"), Space())
      case _ => List()
    }
    val printStatic = if (static) List(Word("static"), Space()) else List()
    val printOverride = if (overrided) List(Word("override"), Space()) else List()
    val printAbs = if (abs) List(Word("abstract"), Space()) else List()
    val printReadonly = if (isField && readonly) List(Word("readonly"), Space()) else List()

    printDeclare ::: printAccess ::: printStatic ::: printOverride ::: printAbs ::: printReadonly
  }
}
