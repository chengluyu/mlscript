package mlscript.codegen.generator

import scala.util.matching.Regex
import scala.collection.mutable.{ArrayBuffer, HashSet}
import mlscript.codegen.{Position, Location, LocationType}
import mlscript.codegen.ast._
import mlscript.codegen.generator._

class CodeGenerator(format: Format, sourceMap: SourceMapBuilder) extends Printer(format, sourceMap) {
  override def print(
    node: Node,
    parent: Option[Node] = None,
    previous: PrintType = PrintType.Empty,
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): PrintType =
    val shouldPrintParens =
      if (forceParens) true
      else Parentheses.needsParens(node, parent, stack)
    if (shouldPrintParens)
      printToken(")", translate(node, parent, printToken("(", previous), forceParens)(indentLevel, stack :+ node))
    else
      translate(node, parent, previous, forceParens)(indentLevel, stack :+ node)

  import CodeGenerator._

  private def translate(
    node: Node,
    parent: Option[Node] = None,
    previous: PrintType = PrintType.Empty,
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): PrintType = node match {
    // BEGIN base.scala
    case File(program) => print(program, Some(node), previous)
    case Program(body, _, _) => printJoin(body, Some(node), previous, statement = true)
    case BlockStatement(body) =>
      printToken("}", printJoin(body, Some(node), printToken("{", previous), statement = true, indent = true))
    // END base.scala
    // ---
    // BEGIN classes.scala
    case ClassDeclaration(id, superClass, body, decorators, abs, dec, implements, superTypeParameters, typeParameters) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printDec = if (dec) printSpace(printWord("declare", printDecoractors)) else printDecoractors
      val printAbs = if (abs) printSpace(printWord("abstract", printDec)) else printDec
      val printID = print(id, Some(node), printWord("class", printAbs)) 
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printID)
        case _ => printID
      }
      val printSuper = (superClass, superTypeParameters) match {
        case (Some(superClass), Some(superTypeParameters)) =>
          print(superTypeParameters, Some(node), print(superClass, Some(node), printList(List(Space(), Word("extends"), Space()), printType)))
        case (Some(superClass), None) =>
          print(superClass, Some(node), printList(List(Space(), Word("extends"), Space()), printType))
        case _ => printType
      }
      val printImplement = implements match {
        case Some(implements) =>
          printJoin(implements, Some(node), printList(List(Space(), Word("implements"), Space()), printSuper), statement = true)
        case _ => printSuper
      }
      print(body, Some(node), printSpace(printImplement))
    case ClassExpression(id, superClass, body, decorators, implements, superTypeParameters, typeParameters) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printID = print(id, Some(node), printWord("class", printDecoractors))
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printID)
        case _ => printID
      }
      val printSuper = (superClass, superTypeParameters) match {
        case (Some(superClass), Some(superTypeParameters)) =>
          print(superTypeParameters, Some(node), print(superClass, Some(node), printList(List(Space(), Word("extends"), Space()), printType)))
        case (Some(superClass), None) =>
          print(superClass, Some(node), printList(List(Space(), Word("extends"), Space()), printType))
        case _ => printType
      }
      val printImplement = implements match {
        case Some(implements) =>
          printJoin(implements, Some(node), printList(List(Space(), Word("implements"), Space()), printSuper), statement = true)
        case _ => printSuper
      }
      print(body, Some(node), printSpace(printImplement))
    case ClassBody(body) =>
      if (body.length == 0) printList(List(Token("{"), Token("}")), previous)
      else 
        val res = printJoin(body, Some(node), printList(List(Token("{"), Newline()), previous), statement = true)(indentLevel + 1, stack)
        res match {
          case PrintType.Newline => printToken("}", res)
          case _ => printList(List(Newline(), Token("}")), res)
        }
    case ClassProperty(key, value, decorators, typeAnnotation, computed, static, abs, access, declare, definite, optional, overrided, readonly) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printModifier = printList(tsPrintClassMemberModifiers(true, declare, access, static, overrided, abs, readonly), printDecoractors)
      val printKey =
        if (computed) printToken("]", print(key, Some(node), printToken("[", printModifier)))
        else print(key, Some(node), printModifier)
      val printOptional = if (optional) printToken("?", printKey) else printKey
      val printDefinite = if (definite) printToken("!", printOptional) else printOptional
      val printType = typeAnnotation match {
        case Some(typeAnnotation) =>
          print(typeAnnotation, Some(node), printDefinite)
        case _ => printDefinite
      }
      printSemicolon(value match {
        case Some(value) =>
          print(value, Some(node), printList(List(Space(), Token("="), Space()), printType))
        case _ => printType
      })
    case ClassAccessorProperty(key, value, decorators, typeAnnotation, computed, static, abs, access, declare, definite, optional, overrided, readonly) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printModifier =
        printList(tsPrintClassMemberModifiers(true, declare, access, static, overrided, abs, readonly) ::: List(Word("accessor"), Space()), printDecoractors)
      val printKey =
        if (computed) printToken("]", print(key, Some(node), printToken("[", printModifier)))
        else print(key, Some(node), printModifier)
      val printOptional = if (optional) printToken("?", printKey) else printKey
      val printDefinite = if (definite) printToken("!", printOptional) else printOptional
      val printType = typeAnnotation match {
        case Some(typeAnnotation) =>
          print(typeAnnotation, Some(node), printDefinite)
        case _ => printDefinite
      }
      printSemicolon(value match {
        case Some(value) =>
          print(value, Some(node), printList(List(Space(), Token("="), Space()), printType))
        case _ => printType
      })
    case ClassPrivateProperty(key, value, decorators, typeAnnotation, static, definite, readonly) =>
      val printDecoractors = printJoin(decorators, Some(node), previous)
      val printStatic = if (static) printList(List(Word("static"), Space()), printDecoractors) else printDecoractors
      val printKey = print(key, Some(node), printStatic)
      val printType = typeAnnotation match {
        case Some(typeAnnotation) =>
          print(typeAnnotation, Some(node), printKey)
        case _ => printKey
      }
      printSemicolon(value match {
        case Some(value) =>
          print(value, Some(node), printList(List(Space(), Token("="), Space()), printType))
        case _ => printType
      })
    case method: ClassMethod =>
      print(method.body, Some(node), printSpace(classMethodHead(method, previous)))
    case method: ClassPrivateMethod =>
      print(method.body, Some(node), printSpace(classMethodHead(method, previous)))
    case StaticBlock(body) =>
      if (body.length == 0) printList(List(Word("static"), Space(), Token("{"), Token("}")), previous)
      else
        printToken("}", printJoin(body, Some(node), printList(List(Word("static"), Space(), Token("{"), Newline()), previous), indent = true))
    // END classes.scala
    // ---
    // BEGIN expressions.scala
    case ThisExpression() =>
      printWord("this", previous)
    case Super() =>
      printWord("super", previous)
    case Import() =>
      printWord("import", previous)
    case EmptyStatement() =>
      printSemicolon(previous)
    case DoExpression(body, async) =>
      val lst =
        if (async) List(Word("async"), Space(), Word("do"), Space())
        else List(Word("do"), Space())
      print(body, Some(node), printList(lst, previous))
    case ParenthesizedExpression(exp) =>
      print(exp, Some(node), printList(List(Token("("), Token(")")), previous))
    case ConditionalExpression(test, consequent, alternate) =>
      val printTest = print(test, Some(node), previous)
      val printConseq = print(consequent, Some(node), printList(List(Space(), Token("?"), Space()), printTest))
      print(alternate, Some(node), printList(List(Space(), Token(":"), Space()), printConseq))
    case Decorator(exp) =>
      val prefix =
        if (shouldParenthesizeDecoratorExpression(exp)) List(Token("@"), Token("("))
        else List(Token("@"))
      val suffix =
        if (shouldParenthesizeDecoratorExpression(exp)) List(Token(")"), Newline())
        else List(Newline())
      printList(suffix, print(exp, Some(node), printList(prefix, previous)))
    case OptionalMemberExpression(obj, prop, computed, optional) =>
      val printObj = print(obj, Some(node), previous)
      (computed, optional) match {
        case (true, true) =>
          printToken("]", print(prop, Some(node), printList(List(Token("?."), Token("[")), printObj)))
        case (true, false) =>
          printToken("]", print(prop, Some(node), printToken("[", printObj)))
        case (false, true) => print(prop, Some(node), printToken("?.", printObj))
        case _ => print(prop, Some(node), printToken(".", printObj))
      }
    case ExpressionStatement(exp) =>
      printSemicolon(print(exp, Some(node), previous))
    case AssignmentPattern(left, right) =>
      val printLeft = print(left, Some(node), previous)
      print(right, Some(node), printList(List(Space(), Token("="), Space()), printLeft))
    case AssignmentExpression(op, left, right) => // TODO: check if parens are necessary
      val needsParens = op.equals("in") && !Parentheses.needsParens(node, parent, List())
      val printLeft = printSpace(print(left, Some(node), if (needsParens) printToken("(", previous) else previous))
      val opPrint = if (op.equals("in") || op.equals("instanceof")) Word(op) else Token(op)
      if (needsParens)
        printToken(")", print(right, Some(node), printList(List(opPrint, Space()), printLeft)))
      else print(right, Some(node), printList(List(opPrint, Space()), printLeft))
    case BindExpression(obj, callee) =>
      val printObj = print(obj, Some(node), previous)
      print(callee, Some(node), printToken("::", printObj))
    case BinaryExpression(op, left, right) =>
      val needsParens = op == BinaryOperator.In && !Parentheses.needsParens(node, parent, List())
      val printLeft = printSpace(print(left, Some(node), if (needsParens) printToken("(", previous) else previous))
      val ops = BinaryOperator.to(op)
      val opPrint = if (ops.equals("in") || ops.equals("instanceof")) Word(ops) else Token(ops)
      if (needsParens)
        printToken(")", print(right, Some(node), printList(List(opPrint, Space()), printLeft)))
      else print(right, Some(node), printList(List(opPrint, Space()), printLeft))
    case LogicalExpression(op, left, right) =>
      val needsParens = op == BinaryOperator.In && !Parentheses.needsParens(node, parent, List())
      val printLeft = printSpace(print(left, Some(node), if (needsParens) printToken("(", previous) else previous))
      val ops = LogicalOperator.to(op)
      val opPrint = if (ops.equals("in") || ops.equals("instanceof")) Word(ops) else Token(ops)
      if (needsParens)
        printToken(")", print(right, Some(node), printList(List(opPrint, Space()), printLeft)))
      else print(right, Some(node), printList(List(opPrint, Space()), printLeft))
    case MemberExpression(obj, prop, computed) =>
      val printObj = print(obj, Some(node), previous)
      if (computed) printToken("]", print(prop, Some(node), printToken("[", printObj)))
      else print(prop, Some(node), printToken(".", printObj))
    case MetaProperty(meta, prop) =>
      val printMeta = print(meta, Some(node), previous)
      print(prop, Some(node), printToken(".", printMeta))
    case PrivateName(id) =>
      print(id, Some(node), printToken("#", previous))
    case V8IntrinsicIdentifier(name) =>
      printList(List(Token("%"), Word(name)), previous)
    case ModuleExpression(body) =>
      val prefix = List(Word("module"), Space(), Token("{"))
      printToken("}", print(body, Some(node), printNewline(1, printList(prefix, previous))(indentLevel + 1))(indentLevel + 1, stack))
    case NewExpression(callee, arguments, _, typeParameters) =>
      val printCallee = print(callee, Some(node), printList(List(Word("new"), Space()), previous))
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printCallee)
        case _ => printCallee
      }
      printToken(")", printJoin(arguments, Some(node), printToken("(", printType), List(Token(","), Space())))
    case SequenceExpression(expressions) =>
      printJoin(expressions, Some(node), previous, List(Token(","), Space()))
    case OptionalCallExpression(callee, arguments, optional, typeParameters) =>
      val printCallee = print(callee, Some(node), previous)
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printCallee)
        case _ => printCallee
      }
      val printOptional = if (optional) printToken("?.", printType) else printType
      printToken(")", printJoin(arguments, Some(node), printToken("(", printOptional), List(Token(","), Space())))
    case CallExpression(callee, arguments, optional, typeParameters) =>
      val printCallee = print(callee, Some(node), previous)
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printCallee)
        case _ => printCallee
      }
      printToken(")", printJoin(arguments, Some(node), printToken("(", printType), List(Token(","), Space())))
    case UpdateExpression(operator, argument, prefix) =>
      if (prefix) print(argument, Some(node), printToken(UpdateOperator.to(operator), previous))
      else printToken(UpdateOperator.to(operator), print(argument, Some(node), previous))
    case AwaitExpression(argument) =>
      print(argument, Some(node), printList(List(Word("await"), Space()), previous))
    case YieldExpression(argument, delegate) =>
      argument match {
        case Some(argument) if (delegate) => print(argument, Some(node), printList(List(Word("yield"), Token("*"), Space()), previous))
        case _ if (delegate) => printList(List(Word("yield"), Token("*")), previous)
        case Some(argument) => print(argument, Some(node), printList(List(Word("yield"), Space()), previous))
        case _ => printWord("yield", previous)
      }
    // END expressions.scala
    // ---
    // BEGIN methods.scala
    case FunctionExpression(id, params, body, generator, asnyc, returnType, typeParameters) =>
      print(body, Some(node),
        printSpace(functionHead(typeParameters, params, returnType, id, Some(node), printSpace(previous), asnyc, generator)))
    case FunctionDeclaration(id, params, body, generator, asnyc, _, returnType, typeParameters) =>
      print(body, Some(node),
        printSpace(functionHead(typeParameters, params, returnType, id, Some(node), printSpace(previous), asnyc, generator)))
    case ArrowFunctionExpression(parameters, body, asnyc, expression, generator, returnType, typeParameter) =>
      val printAsync = if (asnyc) printList(List(Word("async"), Space()), previous) else previous
      val printParam = params(typeParameter, parameters, returnType, Some(node), printAsync)
      print(body, Some(node), printList(List(Space(), Token("=>"), Space()), printParam))
    // END methods.scala
    // ---
    // BEGIN modules.scala
    case ImportSpecifier(local, imported, kind) =>
      val printImported = kind match {
        case ImportKind.Type => print(imported, Some(node), printList(List(Word("type"), Space()), previous))
        case ImportKind.TypeOf => print(imported, Some(node), printList(List(Word("type"), Space()), previous))
        case _ => print(imported, Some(node), previous)
      }
      local match { // TODO: add location information
        case Some(Identifier(localName, typeAnnotation)) => imported match {
          case Identifier(name, _) if (!name.equals(localName)) =>
            print(Identifier(localName, typeAnnotation)(None, None, None), Some(node), printList(List(Space(), Word("as"), Space()), printImported))
          case StringLiteral(str) if (!str.equals(localName)) =>
            print(Identifier(localName, typeAnnotation)(None, None, None), Some(node), printList(List(Space(), Word("as"), Space()), printImported))
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
        case ExportKind.Type => print(exported, Some(node), printList(List(Word("type"), Space()), previous))
        case _ => print(exported, Some(node), previous)
      }
      local match { // TODO: add location information
        case Some(Identifier(localName, typeAnnotation)) => exported match {
          case Identifier(name, _) if (!name.equals(localName)) =>
            print(Identifier(localName, typeAnnotation)(None, None, None), Some(node), printList(List(Space(), Word("as"), Space()), printExported))
          case StringLiteral(str) if (!str.equals(localName)) =>
            print(Identifier(localName, typeAnnotation)(None, None, None), Some(node), printList(List(Space(), Word("as"), Space()), printExported))
          case _ => printExported
        }
        case _ => printExported
      }
    case ExportNamespaceSpecifier(exported) =>
      print(exported, Some(node), printList(List(Token("*"), Space(), Word("as"), Space()), previous))
    case ExportDefaultDeclaration(dec) =>
      val printExported = print(dec, Some(node), printList(List(Word("export"), Space(), Word("default"), Space()), previous))
      dec match {
        case s: Statement => printExported
        case _ => printSemicolon(printExported)
      }
    case ImportAttribute(key, value) =>
      val printKey = print(key, Some(node), previous)
      print(value, Some(node), printList(List(Token(":"), Space()), printKey))
    case ImportNamespaceSpecifier(local) =>
      print(local, Some(node), printList(List(Token("*"), Space(), Word("as"), Space()), previous))
    case ExportAllDeclaration(source, assertions, exportKind) =>
      val printExport =
        if (exportKind == ExportKind.Type) List(Word("export"), Space(), Word("type"), Space())
        else List(Word("export"), Space())
      val printSource = print(source, Some(node), printList(printExport ::: List(Token("*"), Space(), Word("from"), Space()), previous))
      assertions match {
        case Some(assertions) if (assertions.length > 0) =>
          printSemicolon(printAssertions(assertions, Some(node), printSpace(printSource)))
        case _ => printSemicolon(printSource)
      }
    case ExportNamedDeclaration(declaration, specifiers, source,assertions, exportKind) =>
      def run(spec: List[Node], prev: PrintType, hasSpecial: Boolean): (PrintType, List[Node], Boolean) = spec match {
        case first :: rest => first match {
          case _: ExportDefaultSpecifier => 
            run(rest, ((if (rest.length > 0) printList(List(Token(","), Space()), print(first, Some(node), prev)) else print(first, Some(node), prev))), true)
          case _: ExportNamespaceSpecifier =>
            run(rest, ( (if (rest.length > 0) printList(List(Token(","), Space()), print(first, Some(node), prev)) else print(first, Some(node), prev))), true)
          case _ => (prev, spec, hasSpecial)
        }
        case _ => (prev, spec, hasSpecial)
      }

      declaration match {
        case Some(declaration: Statement) =>
          print(declaration, Some(node), printList(List(Word("export"), Space()), previous))
        case Some(declaration) =>
          print(declaration, Some(node), printSemicolon(printList(List(Word("export"), Space()), previous)))
        case _ => {
          val printDeclare =
            if (exportKind == ExportKind.Type) printList(List(Word("export"), Space(), Word("type"), Space()), previous)
            else printList(List(Word("export"), Space()), previous)
          
          val (printSpec, rest, hasSpecial) = run(specifiers, printDeclare, false)
          val printRest =
            if (rest.length > 0) printList(List(Space(), Token("}")), printJoin(rest, Some(node), printList(List(Token("{"), Space()), printSpec), List(Token(","), Space())))
            else if (!hasSpecial) printList(List(Token("{"), Token("}")), printSpec)
            else printSpec
          printSemicolon(source match {
            case Some(source) => assertions match {
              case Some(assertions) if (assertions.length > 0) =>
                printAssertions(assertions, Some(node), 
                  print(source, Some(node), printList(List(Space(), Word("from"), Space()), printRest)))
              case _ => print(source, Some(node), printList(List(Space(), Word("from"), Space()), printRest))
            }
            case _ => printRest
          })
        }
      }
    case ImportDeclaration(specifiers, source, assertions, importKind, module) =>
      val printKind =
        if (importKind == ImportKind.Type)
          printList(List(Word("import"), Space(), Word("type"), Space()), previous)
        else if (importKind == ImportKind.TypeOf)
          printList(List(Word("import"), Space(), Word("typeof"), Space()), previous)
        else if (module)
          printList(List(Word("import"), Space(), Word("module"), Space()), previous)
        else printList(List(Word("import"), Space()), previous)
      def run(spec: List[Node], prev: PrintType): (PrintType, List[Node]) = spec match {
        case first :: rest => first match {
          case _: ImportDefaultSpecifier => 
            run(rest, ((if (rest.length > 0) printList(List(Token(","), Space()), print(first, Some(node), prev)) else print(first, Some(node), prev))))
          case _: ImportNamespaceSpecifier =>
            run(rest, ((if (rest.length > 0) printList(List(Token(","), Space()), print(first, Some(node), prev)) else print(first, Some(node), prev))))
          case _ => (prev, spec)
        }
        case _ => (prev, spec)
      }
      val (printSpec, rest) =
        if (specifiers.length > 0) run(specifiers, printKind)
        else (printKind, specifiers)
      val printRest =
        if (rest.length > 0) printList(List(Space(), Token("}")), printJoin(rest, Some(node), printList(List(Token("{"), Space()), printSpec)))
        else if (importKind == ImportKind.Type || importKind == ImportKind.TypeOf) printList(List(Token("{"), Token("}")), printSpec)
        else printSpec
      val printFrom =
        if (specifiers.length > 0 || importKind == ImportKind.Type || importKind == ImportKind.TypeOf)
          printList(List(Space(), Word("from"), Space()), printRest)
        else printRest
      printSemicolon(assertions match {
        case Some(assertions) =>
          printAssertions(assertions, Some(node), printSpace(print(source, Some(node), printFrom)))
        case _ => print(source, Some(node), printFrom)
      })
    // END modules.scala
    // ---
    // BEGIN statements.scala
    case WithStatement(obj, body) =>
      val printObj = print(obj, Some(node), printList(List(Word("with"), Space(), Token("(")), previous))
      printBlock(body, Some(node), printToken(")", printObj))
    case ForStatement(init, test, update, body) =>
      val printInit = init match {
        case Some(init) => printToken(";", print(init, Some(node), printList(List(Word("for"), Space(), Token("(")), previous)))
        case _ => printList(List(Word("for"), Space(), Token("("), Token(";")), previous)
      }
      val printTest = test match {
        case Some(test) => printToken(";", print(test, Some(node), printSpace(printInit)))
        case _ => printToken(";", printInit)
      }
      val printUpdate = update match {
        case Some(update) => print(update, Some(node), printSpace(printTest))
        case _ => printTest
      }
      printBlock(body, Some(node), printToken(")", printUpdate))
    case WhileStatement(test, body) =>
      val printTest = printToken(")", print(test, Some(node), printList(List(Word("while"), Space(), Token("(")), previous)))
      printBlock(body, Some(node), printTest)
    case ForInStatement(left, right, body) =>
      val printLeft = print(left, Some(node), printList(List(Word("for"), Space(), Token("(")), previous))
      val printRight = printToken(")", print(right, Some(node), printList(List(Word("in"), Space()), previous)))
      printBlock(body, Some(node), printRight)
    case ForOfStatement(left, right, body, await) =>
      val prefix =
        if (await) List(Word("for"), Space(), Word("await"), Space(), Token("("))
        else List(Word("for"), Space(), Token("("))
      val printLeft = print(left, Some(node), printList(prefix, previous))
      val printRight = printToken(")", print(right, Some(node), printList(List(Word("of"), Space()), printLeft)))
      printBlock(body, Some(node), printRight)
    case DoWhileStatement(test, body) =>
      val printBody = print(body, Some(node), printList(List(Word("do"), Space()), previous))
      printList(List(Token(")"), Semicolon()), print(test, Some(node), printList(List(Space(), Word("while"), Space(), Token("(")), printBody)))
    case LabeledStatement(label, body) =>
      val printLabel = printList(List(Token(":"), Space()), print(label, Some(node), previous))
      print(body, Some(node), printLabel)
    case TryStatement(block, handler, finalizer) =>
      val printBlock = printSpace(print(block, Some(node), printList(List(Word("try"), Space()), previous)))
      val printHandler = handler match {
        case Some(handler) => print(handler, Some(node), printBlock)
        case _ => printBlock
      }
      finalizer match {
        case Some(finalizer) => print(finalizer, Some(node), printList(List(Space(), Word("finally"), Space()), printHandler))
        case _ => printHandler
      }
    case CatchClause(param, body) =>
      val prefix = List(Word("catch"), Space())
      val printParam = param match {
        case Some(id: Identifier) => id.typeAnnotation match {
          case Some(typeAnnotation) =>
            printList(List(Token(")"), Space()), print(typeAnnotation, Some(node), print(id, Some(node), printToken("(", printList(prefix, previous)))))
          case _ =>
            printList(List(Token(")"), Space()), print(id, Some(node), printToken("(", printList(prefix, previous))))
        }
        case Some(pat: ArrayPattern) => pat.typeAnnotation match {
          case Some(typeAnnotation) =>
            printList(List(Token(")"), Space()), print(typeAnnotation, Some(node), print(pat, Some(node), printToken("(", printList(prefix, previous)))))
          case _ =>
            printList(List(Token(")"), Space()), print(pat, Some(node), printToken("(", printList(prefix, previous))))
        }
        case Some(pat: ObjectPattern) => pat.typeAnnotation match {
          case Some(typeAnnotation) =>
            printList(List(Token(")"), Space()), print(typeAnnotation, Some(node), print(pat, Some(node), printToken("(", printList(prefix, previous)))))
          case _ =>
            printList(List(Token(")"), Space()), print(pat, Some(node), printToken("(", printList(prefix, previous))))
        }
        case _ => printList(prefix, previous)
      }
      print(body, Some(node), printParam)
    case DebuggerStatement() =>
      printList(List(Word("debugger"), Semicolon()), previous)
    case VariableDeclarator(id, init, definite) =>
      val printId = (if (definite) printToken("!", print(id, Some(node), previous)) else print(id, Some(node), previous))
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
        case Some(init) => print(init, Some(node), printList(List(Space(), Token("="), Space()), printAnnotation))
        case _ => printAnnotation
      }
    case IfStatement(test, consequent, alternate) =>
      val printTest = printList(List(Token(")"), Space()), print(test, Some(node), printList(List(Word("if"), Space(), Token("(")), previous)))
      val needsBlock = alternate.isDefined // TODO: check last statement
      val newIndent = if (needsBlock) indentLevel + 1 else indentLevel
      val printConseq =
        printList(
          (if (needsBlock) List(Newline(), Token("}")) else List()),
          print(consequent, Some(node), printList((if (needsBlock) List(Token("{"), Newline()) else List()), printTest)(newIndent))(newIndent, stack)
        )
      alternate match {
        case Some(alternate) =>
          print(alternate, Some(node), printList(List(Space(), Word("else"), Space()), printConseq))
        case _ => printConseq
      }
    case SwitchStatement(discriminant, cases) =>
      val printDis = printList(List(Token(")"), Space(), Token("{")), print(discriminant, Some(node), printList(List(Word("switch"), Space(), Token("(")), previous)))
      printToken("}", printJoin(cases, Some(node), printDis, indent = true, statement = true))
    case SwitchCase(test, consequent) =>
      val printTest = test match {
        case Some(test) =>
          printToken(":", print(test, Some(node), printList(List(Word("case"), Space()), previous)))
        case _ => printList(List(Word("default"), Token(":")), previous)
      }
      if (consequent.length > 0)
        printJoin(consequent, Some(node), printNewline(1, printTest), indent = true, statement = true)
      else printTest
    case VariableDeclaration(kind, declarations, declare) =>
      val printDeclare = if (declare) printList(List(Word("declare"), Space()), previous) else previous
      val printKind = kind match {
        case VariableDeclarationKind.Var => printList(List(Word("var"), Space()), printDeclare)
        case VariableDeclarationKind.Let => printList(List(Word("let"), Space()), printDeclare)
        case VariableDeclarationKind.Const => printList(List(Word("const"), Space()), printDeclare)
        case VariableDeclarationKind.Using => printList(List(Word("using"), Space()), printDeclare)
      }
      val isFor = parent match {
        case Some(_: For) => true
        case _ => false
      }
      val hasInits =
        if (isFor) false else declarations.foldLeft(false)((res, dec) => if (dec.init.isDefined) true else res)
      val sep = if (hasInits) List(Token(","), Newline()) else List()
      val printDec = printJoin(declarations, Some(node), printKind, sep, indent = declarations.length > 1)
      if (isFor) parent match {
        case Some(ForStatement(Some(init), _, _, _)) if (init == (node)) => printDec
        case Some(ForInStatement(left, _, _)) if (left == (node)) => printDec
        case Some(ForOfStatement(left, _, _, _)) if (left == (node)) => printDec
        case _ => printSemicolon(printDec)
      }
      else printDec
    case BreakStatement(label) =>
      printStatementAfterKeyword(label, Some(node), printWord("break", previous))
    case ContinueStatement(label) =>
      printStatementAfterKeyword(label, Some(node), printWord("continue", previous))
    case ReturnStatement(argument) =>
      printStatementAfterKeyword(argument, Some(node), printWord("return", previous))
    case ThrowStatement(argument) =>
      printSemicolon(print(argument, Some(node), printList(List(Space(), Word("throw")), previous)))
    // END statements.scala
    // ---
    // BEGIN templates.scala
    case te @ TemplateElement(value, _) =>
      parent match {
        case Some(TemplateLiteral(quasi, _)) =>
          val isFirst = !quasi.isEmpty && quasi.head == (te)
          val isLast = !quasi.isEmpty && quasi.last == (te)
          printToken(s"${if (isFirst) "`" else "}"}${value}${if (isLast) "`" else "${"}", previous, true)
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
      printWord(name, previous)
    case ArgumentPlaceholder() =>
      printToken("?", previous)
    case RegExpLiteral(pattern, flags) =>
      printWord(s"/$$$pattern/$$$flags", previous)
    case BooleanLiteral(value) =>
      if (value) printWord("true", previous) else printWord("false", previous)
    case NullLiteral() =>
      printWord("null", previous)
    case NumericLiteral(value) =>
      printNumber(value.toString(), previous)
    case StringLiteral(value) =>
      printToken(s"\"${value}\"", previous)
    case BigIntLiteral(value) =>
      printWord(s"${value}n", previous)
    case DecimalLiteral(value) =>
      printWord(s"${value}m", previous)
    case TopicReference() =>
      printToken("#", previous) // TODO: add settings
    case PipelinePrimaryTopicReference() =>
      printToken("#", previous)
    case RestElement(arg, _, _, _) =>
      print(arg, Some(node), printToken("...", previous))
    case ArrayExpression(elements) =>
      printToken("]", elements.iterator.zipWithIndex.foldLeft(printToken("[", previous))((prev, pair) => pair match {
        case (Some(ele), i) =>
          val p = print(ele, Some(node), if (i > 0) printSpace(prev) else prev)
          if (i < elements.length) printToken(",", p) else p
        case _ => printToken(",", prev)
      }))
    case TupleExpression(elements) => // TODO: add settings
      printToken("]", elements.iterator.zipWithIndex.foldLeft(printToken("#[", previous))((prev, pair) => pair match {
        case (ele, i) =>
          val p = print(ele, Some(node), if (i > 0) printSpace(prev) else prev)
          if (i < elements.length) printToken(",", p) else p
      }))
    case PipelineTopicExpression(expression) =>
      print(expression, Some(node), previous)
    case PipelineBareFunction(callee) =>
      print(callee, Some(node), previous)
    case ObjectExpression(props) =>
      if (props.length > 0)
        printList(List(Space(), Token("}")), printJoin(props, Some(node), printToken("{", previous), List(Token(","), Space()), true, true))
      else printList(List(Token("{"), Token("}")), previous)
    case ObjectMethod(kind, key, params, body, computed, generator, async, decorators, returnType, typeParameters) =>
      val printDec = printJoin(decorators, Some(node), previous)
      val printHead = methodHead(typeParameters, params, returnType, Some(node), printDec,
        kind match {
          case ObjectMethodKind.Method => "method"
          case ObjectMethodKind.Init => "init"
          case ObjectMethodKind.Getter => "get"
          case ObjectMethodKind.Setter => "set"
        },
        key, async, generator, computed, false)
      print(body, Some(node), printSpace(printHead))
    case ObjectProperty(key, value, computed, shorthand, decorators) =>
      val printDec = printJoin(decorators, Some(node), previous)
      val printKey =
        if (computed) printToken("]", print(key, Some(node), printToken("[", printDec)))
        else print(key, Some(node), printDec)
      (key, value) match {
        case (Identifier(name1, _), AssignmentPattern(Identifier(name2, _), _)) if (name1.equals(name2)) =>
          print(value, Some(node), printDec)
        case (Identifier(name1, _), Identifier(name2, _)) if (name1.equals(name2) && shorthand) => printKey
        case _ => print(value, Some(node), printList(List(Token(":"), Space()), printKey))
      }
    case RecordExpression(props) =>
      if (props.length > 0)
        printList(List(Space(), Token("}")), printJoin(props, Some(node), printList(List(Token("#{"), Space()), previous), List(Token(","), Space()), true, true))
      else printList(List(Token("#{"), Token("}")), previous)
    // END types.scala
    // ---
    // BEGIN typescript.scala
    case TSAnyKeyword() => printWord("any", previous)
    case TSBigIntKeyword() => printWord("bigint", previous)
    case TSUnknownKeyword() => printWord("unknown", previous)
    case TSNumberKeyword() => printWord("number", previous)
    case TSObjectKeyword() => printWord("object", previous)
    case TSBooleanKeyword() => printWord("boolean", previous)
    case TSStringKeyword() => printWord("string", previous)
    case TSSymbolKeyword() => printWord("symbol", previous)
    case TSVoidKeyword() => printWord("void", previous)
    case TSUndefinedKeyword() => printWord("undefined", previous)
    case TSNullKeyword() => printWord("null", previous)
    case TSNeverKeyword() => printWord("never", previous)
    case TSIntrinsicKeyword() => printWord("intrinsic", previous)
    case TSThisType() => printWord("this", previous)
    case TSTypeAnnotation(typeAnnotation) =>
      print(typeAnnotation, Some(node), printList(List(Token(":"), Space()), previous))
    case TSTypeParameter(constraint, default, name, in, out) =>
      val printIn = if (in) List(Word("in"), Space()) else List()
      val printOut = if (out) List(Word("out"), Space()) else List()
      val printConstraint = constraint match {
        case Some(constraint) =>
          print(constraint, Some(node), printList(printIn ::: printOut ::: List(Word(name), Space(), Word("extends"), Space()), previous))
        case _ => printList((printIn ::: printOut) :+ Word(name), previous)
      }
      default match {
        case Some(default) =>
          print(default, Some(node), printList(List(Space(), Token("="), Space()), printConstraint))
        case _ => printConstraint
      }
    case TSQualifiedName(left, right) =>
      print(right, Some(node), printToken(".", print(left, Some(node), previous)))
    case sig @ TSPropertySignature(key, typeAnnotation, init, _, _, _, readonly) =>
      val prefix = if (readonly) printList(List(Word("readonly"), Space()), previous) else previous
      val printProperty = tsPrintPropertyOrMethodName(sig, parent, prefix)
      val printType = typeAnnotation match {
        case Some(typeAnnotation) => print(typeAnnotation, Some(node), printProperty)
        case _ => printProperty
      }
      printToken(";", init match {
        case Some(init) => print(init, Some(node), printList(List(Space(), Token("="), Space()), printType))
        case _ => printType
      })
    case TSTypeReference(typeName, typeParameter) => typeParameter match {
      case Some(typeParameter) =>
        print(typeParameter, Some(node), print(typeName, Some(node), previous, true), true)
      case _ => print(typeName, Some(node), previous, true)
    }
    case TSTypePredicate(parameterName, typeAnnotation, asserts) =>
      val prefix = if (asserts) printList(List(Word("asserts"), Space()), previous) else previous
      val printName = print(parameterName, Some(node), prefix)
      typeAnnotation match {
        case Some(TSTypeAnnotation(typeAnnotation)) =>
          print(typeAnnotation, None, printList(List(Space(), Word("is"), Space()), printName))
        case _ => printName
      }
    case TSTypeQuery(exprName, typeParameter) =>
      val printName = print(exprName, Some(node), printList(List(Word("typeof"), Space()), previous))
      typeParameter match {
        case Some(typeParameter) => print(typeParameter, Some(node), printName)
        case _ => printName
      }
    case TSTypeLiteral(members) => tsPrintBraced(members, Some(node), previous)
    case TSArrayType(elementType) => printToken("[]", print(elementType, Some(node), previous))
    case TSOptionalType(typeAnnotation) => printToken("?", print(typeAnnotation, Some(node), previous))
    case TSRestType(typeAnnotation) => print(typeAnnotation, Some(node), printToken("...", previous))
    case TSNamedTupleMember(label, elementType, optional) =>
      val printLabel = (if (optional) printToken("?", print(label, Some(node), previous)) else print(label, Some(node), previous))
      print(elementType, Some(node), printList(List(Token(":"), Space()), printLabel))
    case TSConditionalType(checkType, extendsType, trueType, falseType) =>
      printList( List(Space(), Token(":"), Space()), print(falseType, Some(node),
        printList(List(Space(), Token("?"), Space()), print(trueType, Some(node),
          print(extendsType, Some(node),
            printList(List(Space(), Word("extends"), Space()), print(checkType, Some(node), previous)))))))
    case TSInferType(typeParameter) =>
      print(typeParameter, Some(node), printList(List(Token("infer"), Space()), previous))
    case TSParenthesizedType(typeAnnotation) =>
      printToken(")", print(typeAnnotation, Some(node), printToken("(", previous)))
    case TSTypeOperator(typeAnnotation, operator) =>
      print(typeAnnotation, Some(node), printList(List(Word(operator), Space()), previous))
    case TSIndexedAccessType(objectType, indexType) =>
      printToken("]", print(indexType, Some(node), printToken("[", print(objectType, Some(node), previous, true))))
    case TSMappedType(typeParameter, typeAnnotation, nameType, optional, readonly) =>
      val prefix = List(Token("{"), Space()) :::
        (if (readonly.isDefined) tokenIfPlusMinus(readonly) ::: List(Word("readonly"), Space(), Token("[")) else List(Token("[")))
      val printConstraint = typeParameter.constraint match {
        case Some(constraint) =>
          print(constraint, Some(typeParameter), printList(prefix ::: List(Word(typeParameter.name), Space(), Word("in"), Space()), previous))
        case _ => printList(prefix ::: List(Word(typeParameter.name), Space(), Word("in"), Space()), previous)
      }
      val printName = printToken("]", nameType match {
        case Some(nameType) => print(nameType, Some(node), printList(List(Space(), Word("as"), Space()), printConstraint))
        case _ => printConstraint
      })
      val printOptional = if (optional.isDefined) tokenIfPlusMinus(optional) :+ Token("?") else List()
      printList(List(Space(), Token("}")), typeAnnotation match {
        case Some(typeAnnotation) =>
          print(typeAnnotation, Some(node), printList(printOptional ::: List(Token(":"), Space()), printName))
        case _ => printList(printOptional ::: List(Token(":"), Space()), printName)
      })
    case TSLiteralType(literal) => print(literal, Some(node), previous)
    case TSExpressionWithTypeArguments(expr, typeParameters) => typeParameters match {
      case Some(typeParameters) => print(typeParameters, Some(node), print(expr, Some(node), previous))
      case _ => print(expr, Some(node), previous)
    }
    case TSInterfaceBody(body) => tsPrintBraced(body, Some(node), previous)
    case TSTypeAliasDeclaration(id, typeParameter, typeAnnotation, declare) =>
      val prefix = if (declare) List(Word("declare"), Space(), Word("type"), Space()) else List(Word("type"), Space())
      val printID = print(id, Some(node), printList(prefix, previous))
      val printType = typeParameter match {
        case Some(typeParameter) => print(typeParameter, Some(node), printID)
        case _ => printID
      }
      printToken(";", print(typeAnnotation, Some(node), printList(List(Space(), Token("="), Space()), printType)))
    case TSAsExpression(expression, typeAnnotation) =>
      print(typeAnnotation, Some(node), printList(List(Space(), Word("as"), Space()), print(expression, Some(node), previous)))
    case TSSatisfiesExpression(expression, typeAnnotation) =>
      print(typeAnnotation, Some(node), printList(List(Space(), Word("satisfies"), Space()), print(expression, Some(node), previous)))
    case TSTypeAssertion(typeAnnotation, expression) =>
      print(expression, Some(node), printList(List(Token(">"), Space()), print(typeAnnotation, Some(node), printToken("<", previous))))
    case TSInstantiationExpression(expression, typeParameters) => typeParameters match {
      case Some(typeParameters) =>
        print(typeParameters, Some(node), print(expression, Some(node), previous))
      case _ => print(expression, Some(node), previous)
    }
    case TSEnumDeclaration(id, members, const, declare, _) =>
      val printDeclare = if (declare) List(Word("declare"), Space()) else List()
      val printConst = if (const) List(Word("const"), Space()) else List()
      val printID = print(id, Some(node), printList(printDeclare ::: printConst ::: List(Word("enum"), Space()), previous))
      tsPrintBraced(members, Some(node), printSpace(printID))
    case TSEnumMember(id, init) =>
      val printID = print(id, Some(node))
      printToken(",", init match {
        case Some(init) => print(init, Some(node), printList(List(Space(), Token("="), Space()), printID))
        case _ => printID
      })
    case TSModuleDeclaration(id, body, declare, global) =>
      val printDeclare = if (declare) List(Word("declare"), Space()) else List()
      val printGlobal = if (!global) (id match {
        case _: Identifier => List(Word("namespace"), Space())
        case _ => List(Word("module"), Space())
      }) else List()
      val printID = print(id, Some(node), printList(printDeclare ::: printGlobal, previous))
      def printBody(body: TSModuleBlock | TSModuleDeclaration, prev: PrintType): (PrintType, TSModuleBlock) = body match {
        case TSModuleDeclaration(id, sub, _, _) => printBody(sub, print(id, Some(body), printToken(".", prev)))
        case block: TSModuleBlock => (prev, block)
      }
      printBody(body, printID) match {
        case (res, block) => print(block, Some(node), printSpace(res))
      }
    case TSModuleBlock(body) => tsPrintBraced(body, Some(node), previous)
    case TSImportType(argument, qualifier, typeParameters) =>
      val printArgument = printToken(")", print(argument, Some(node), printList(List(Word("import"), Token("(")), previous)))
      val printQualifier = qualifier match {
        case Some(qualifier) => print(qualifier, Some(node), printToken(".", printArgument))
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
      val printID = print(id, Some(node), printList(prefix, previous))
      printToken(";", print(module, Some(node), printList(List(Space(), Token("="), Space()), printID)))
    case TSExternalModuleReference(exp) =>
      printToken(")", print(exp, Some(node), printToken("require(", previous)))
    case TSNonNullExpression(exp) => printToken("!", print(exp, Some(node), previous))
    case TSExportAssignment(exp) =>
      printToken(";", print(exp, Some(node), printList(List(Word("export"), Space(), Token("="), Space()), previous)))
    case TSNamespaceExportDeclaration(id) =>
      print(id, Some(node), printList(List(Word("export"), Space(), Word("as"), Space(), Word("namespace"), Space()), previous))
    case TSTypeParameterInstantiation(params) =>
      val printParams = printJoin(params, Some(node), printToken("<", previous), List(Token(","), Space()))
      parent match {
        case Some(_: ArrowFunctionExpression) if (params.length == 1) =>
          printList(List(Token(","), Token(">")), printParams)
        case _ => printToken(">", printParams)
      }
    case TSParameterProperty(parameter, accessibility, decorators, overrided, readonly) =>
      val printAccess = accessibility match {
        case Some(AccessModifier.Public) => List(Word("public"), Space())
        case Some(AccessModifier.Private) => List(Word("private"), Space())
        case Some(AccessModifier.Protected) => List(Word("protected"), Space())
        case _ => List()
      }
      if (readonly) param(parameter, Some(node), printList(printAccess ::: List(Word("readonly"), Space()), previous))
      else param(parameter, Some(node), printList(printAccess, previous))
    case TSDeclareFunction(id, typeParameters, params, returnType, asnyc, declare, generator) =>
      if (declare)
        printToken(";", functionHead(typeParameters, params, returnType, id, Some(node), printList(List(Word("declare"), Space()), previous), asnyc, generator))
      else
        printToken(";", functionHead(typeParameters, params, returnType, id, Some(node), previous, asnyc, generator))
    case method: TSDeclareMethod => printToken(";", classMethodHead(method, previous))
    case TSCallSignatureDeclaration(typeParameter, params, typeAnnotation) =>
      printToken(";", tsPrintSignatureDeclarationBase(typeParameter, params, typeAnnotation, Some(node), previous))
    case TSConstructSignatureDeclaration(typeParameter, params, typeAnnotation) =>
      printToken(";", tsPrintSignatureDeclarationBase(typeParameter, params, typeAnnotation, Some(node), printList(List(Word("new"), Space()), previous)))
    case sig @ TSMethodSignature(key, typeParameters, parameters, typeAnnotation, kind, computed, optional) =>
      val printKind = kind match {
        case TSMethodSignatureKind.Method => previous
        case TSMethodSignatureKind.Getter => printList(List(Word("get"), Space()), previous)
        case TSMethodSignatureKind.Setter => printList(List(Word("set"), Space()), previous)
      }
      printToken(";", tsPrintSignatureDeclarationBase(typeParameters, parameters, typeAnnotation, Some(node),
        tsPrintPropertyOrMethodName(sig, Some(node), printKind)))
    case TSIndexSignature(params, typeAnnotation, readonly, static) =>
      val printStatic = if (static) List(Word("static"), Space()) else List()
      val printReadonly = if (readonly) List(Word("readonly"), Space()) else List()
      typeAnnotation match {
        case Some(typeAnnotation) =>
          printToken(";", print(typeAnnotation, Some(node),
            parameters(params, Some(node), printList(List(Token("["), Token("]")), printList(printStatic ::: printReadonly, previous)))
          ))
        case _ =>
          printList(List(Token("]"), Token(";")), parameters(params, Some(node), printToken("[", printList(printStatic ::: printReadonly, previous))))
      }
    case TSFunctionType(typeParameters, params, typeAnnotation) =>
      tsPrintFunctionOrConstructorType(typeParameters, params, typeAnnotation, Some(node), previous)
    case TSConstructorType(typeParameters, params, typeAnnotation, abs) =>
      if (abs)
        tsPrintFunctionOrConstructorType(typeParameters, params, typeAnnotation, Some(node), printList(List(Word("abstract"), Space(), Word("new"), Space()), previous))
      else
        tsPrintFunctionOrConstructorType(typeParameters, params, typeAnnotation, Some(node), printList(List(Word("new"), Space()), previous))
    case TSUnionType(types) =>
      printJoin(types, Some(node), previous, List(Space(), Token("|"), Space()))
    case TSIntersectionType(types) =>
      printJoin(types, Some(node), previous, List(Space(), Token("&"), Space()))
    case TSInterfaceDeclaration(id, typeParameters, ext, body, declare) =>
      val printID =
        if (declare) print(id, Some(node), printList(List(Word("declare"), Space(), Word("interface"), Space()), previous))
        else print(id, Some(node), printList(List(Word("interface"), Space()), previous))
      val printType = typeParameters match {
        case Some(typeParameters) => print(typeParameters, Some(node), printID)
        case _ => printID
      }
      val printExt =
        if (ext.length > 0)
          printJoin(ext, Some(node), printList(List(Space(), Word("extends"), Space()), printType), List(Token(","), Space()))
        else printType
      print(body, Some(node), printSpace(printExt))
    // END typescript.scala
  }

  private def printBlock(
    body: Node,
    node: Option[Node] = None,
    previous: PrintType,
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): PrintType = body match {
    case _: EmptyStatement => print(body, node, previous)
    case _ => print(body, node, printSpace(previous))
  }

  private def tsPrintPropertyOrMethodName(
    node: TSPropertySignature | TSMethodSignature,
    parent: Option[Node] = None,
    previous: PrintType,
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): PrintType = node match {
    case TSPropertySignature(key, _, _, _, computed, optional, _) =>
      printList((if (computed) List(Token("]")) else List()) :::
        (if (optional) List(Token("?")) else List()), print(key, Some(node), previous))
    case TSMethodSignature(key, _, _, _, _, computed, optional) =>
      printList((if (computed) List(Token("]")) else List()) :::
        (if (optional) List(Token("?")) else List()), print(key, Some(node), previous))
  }

  private def tsPrintBraced(
    members: List[Node],
    node: Option[Node] = None,
    previous: PrintType,
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): PrintType =
    printToken("}", members.foldLeft(printNewline(1, printToken("{", previous))(indentLevel + 1))(
      (prev, mem) => printNewline(1, print(mem, node, prev)(indentLevel + 1, stack))(indentLevel + 1)))

  private def printJoin(
    nodes: List[Node],
    parent: Option[Node],
    previous: PrintType = PrintType.Empty,
    separator: List[PrintCommand] = List(),
    statement: Boolean = false,
    indent: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node]): PrintType = {
    val newIndent = if (indent) indentLevel + 1 else indentLevel
    nodes.iterator.zipWithIndex.foldLeft(previous)((prev, pair) => pair match {
      case (node, i) =>
        val prefix = if (statement && i == 0) printNewline(1, prev)(newIndent) else prev
        val printNode = print(node, parent, prefix)(newIndent, stack)
        val printSep = if (i + 1 < nodes.length) printList(separator, printNode)(newIndent) else printNode
        if (statement) printNewline(1, printSep)(newIndent) else printSep
    })
  }

  private def param(
    parameter: Node,
    parent: Option[Node],
    previous: PrintType,
    decorators: List[Decorator] = List(),
    optional: Boolean = false,
    typeAnnotation: Option[TSTypeAnnotation] = None,
  )(implicit indentLevel: Int, stack: List[Node]): PrintType = {
    val printDecoractors = printJoin(decorators, Some(parameter), previous)
    val printParam =
      if (optional) printToken("?", print(parameter, parent, printDecoractors))
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
    previous: PrintType,
  )(implicit indentLevel: Int, stack: List[Node]): PrintType =
    params.iterator.zipWithIndex.foldLeft(previous)((prev, pair) => pair match {
      case (p, i) =>
        val printParam = (p match {
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
        })
        if (i + 1 < params.length) printList(List(Token(","), Space()), printParam) else printParam
    })

  private def params(
    typeParameter: Option[Node],
    params: List[Identifier | RestElement | Node with Pattern | TSParameterProperty],
    returnType: Option[Node],
    parent: Option[Node] = None,
    previous: PrintType
  )(implicit indentLevel: Int, stack: List[Node]): PrintType =
    (typeParameter, returnType) match {
      case (Some(typeParameter), Some(returnType)) =>
        print(returnType, parent, printToken(")", parameters(params, parent, printToken("(", print(typeParameter, parent, previous)))))
      case (None, Some(returnType)) =>
        print(returnType, parent, printToken(")", parameters(params, parent, printToken("(", previous))))
      case (Some(typeParameter), None) =>
        printToken(")", parameters(params, parent, printToken("(", print(typeParameter, parent, previous))))
      case _ => printToken(")", parameters(params, parent, printToken("(", previous)))
    }
    
  
  private def methodHead(
    typeParameter: Option[Node],
    parameters: List[Identifier | RestElement | Node with Pattern | TSParameterProperty],
    returnType: Option[Node],
    parent: Option[Node],
    previous: PrintType,
    kind: "get" | "set" | "method" | "init" | "constructor",
    key: Node,
    async: Boolean = false,
    generator: Boolean = false,
    computed: Boolean = false,
    optional: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node]): PrintType = {
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
      if (computed) printToken("]", print(key, parent, printList(printGenerator :+ Token("["), previous)))
      else print(key, parent, printList(printGenerator, previous))
    params(typeParameter, parameters, returnType, parent, if (optional) printToken("?", printKey) else printKey)
  }

  private def classMethodHead(
    node: ClassMethod | ClassPrivateMethod | TSDeclareMethod,
    previous: PrintType = PrintType.Empty
  )(implicit indentLevel: Int, stack: List[Node]): PrintType = node match {
    case ClassMethod(kind, key, params, _, computed, static, generator, async, abs, access,
      decorators, optional, overrided, returnType, typeParameter) =>
        methodHead(typeParameter, params, returnType, Some(node),
          printList(tsPrintClassMemberModifiers(false, false, access, static, overrided, abs, false), printJoin(decorators, Some(node), previous)),
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
          printList(tsPrintClassMemberModifiers(false, false, access, static, overrided, abs, false), printJoin(decorators, Some(node), previous)),
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
          printList(tsPrintClassMemberModifiers(false, false, access, static, overrided, abs, false), printJoin(decorators, Some(node), previous)),
          kind match {
            case TSDeclareMethodKind.Method => "method"
            case TSDeclareMethodKind.Constructor => "constructor"
            case TSDeclareMethodKind.Getter => "get"
            case TSDeclareMethodKind.Setter => "set"
          },
          key, async, generator, computed, optional
        )
  }

  private def functionHead(
    typeParameter: Option[Node],
    parameters: List[Identifier | RestElement | Node with Pattern | TSParameterProperty],
    returnType: Option[Node],
    id: Option[Node],
    parent: Option[Node] = None,
    previous: PrintType,
    async: Boolean = false,
    generator: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node]): PrintType = {
    val printFunc = if (async) printList(List(Word("async"), Space(), Word("function")), previous) else printWord("function", previous)
    val printGenerator = if (generator) printToken("*", printFunc) else printFunc
    val printID = id match {
      case Some(id) => print(id, parent, printSpace(printGenerator))
      case _ => printGenerator
    }
    params(typeParameter, parameters, returnType, parent, printID)
  }

  private def printAssertions(
    assertions: List[Node],
    parent: Option[Node],
    previous: PrintType
  )(implicit indentLevel: Int, stack: List[Node]): PrintType =
    printList(List(Space(), Token("}")), printJoin(assertions, parent,
      printList(List(Word("assert"), Space(), Token("{"), Space()), previous),
      List(Token(","), Space())))

  private def tsPrintSignatureDeclarationBase(
    typeParameter: Option[Node],
    params: List[Identifier | RestElement | Node with Pattern | TSParameterProperty],
    returnType: Option[Node],
    parent: Option[Node] = None,
    previous: PrintType
  )(implicit indentLevel: Int, stack: List[Node]): PrintType =
    (typeParameter, returnType) match {
      case (Some(typeParameter), Some(returnType)) =>
        print(returnType, parent, printToken(")", parameters(params, parent, printToken("(", print(typeParameter, parent, previous)))))
      case (Some(typeParameter), None) =>
        printToken(")", parameters(params, parent, printToken("(", print(typeParameter, parent, previous))))
      case (None, Some(returnType)) =>
        print(returnType, parent, printToken(")", parameters(params, parent, printToken("(", previous))))
      case _ => printToken(")", parameters(params, parent, printToken("(", previous)))
    }
  
  private def tsPrintFunctionOrConstructorType(
    typeParameter: Option[Node],
    params: List[Identifier | RestElement | Node with Pattern | TSParameterProperty],
    returnType: Option[Node],
    parent: Option[Node] = None,
    previous: PrintType
  )(implicit indentLevel: Int, stack: List[Node]): PrintType =
    (typeParameter, returnType) match {
      case (Some(typeParameter), Some(returnType)) =>
        print(returnType, parent, printList(List(Token(")"), Space(), Token("=>"), Space()), printToken("(", parameters(params, parent, print(typeParameter, parent, previous)))))
      case (Some(typeParameter), None) =>
        printList(List(Token(")"), Space(), Token("=>"), Space()), parameters(params, parent, printToken("(", print(typeParameter, parent, previous))))
      case (None, Some(returnType)) =>
        print(returnType, parent, printList(List(Token(")"), Space(), Token("=>"), Space()), parameters(params, parent, printToken("(", previous))))
      case _ => printList(List(Token(")"), Space(), Token("=>"), Space()), parameters(params, parent, printToken("(", previous)))
    }

  private def printStatementAfterKeyword(
    node: Option[Node],
    parent: Option[Node],
    previous: PrintType
  )(implicit indentLevel: Int, stack: List[Node]): PrintType = node match {
    case Some(node) =>
      printSemicolon(print(node, parent, printSpace(previous)))
    case _ => printSemicolon(previous)
  }
}

object CodeGenerator {
  def apply(format: Format, sourceMap: SourceMapBuilder) = new CodeGenerator(format, sourceMap)

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
      case Semicolon() => Semicolon()
      case Space() => Space()
      case Word(str) => Word(str)
      case Number(str) => Number(str)
      case Token(str, maybeNewline) => Token(str, maybeNewline)
      case Newline(i) => Newline(i)
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
