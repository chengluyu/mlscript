package mlscript.codegen.generator

import mlscript.codegen.LocationType
import mlscript.codegen.ast._
import mlscript.codegen.LocationType
import mlscript.codegen.generator._

class CodeGenerator(
  format: Format,
  sourceMap: SourceMapBuilder
):
  private val buffer = new Buffer(Some(sourceMap))

  // TODO: add source mapping & check format
  def generate(commands: List[PrintCommand], lastCmd: Option[PrintCommand] = None): Unit = commands match {
    case Semicolon(force) :: tail =>
      if (force) buffer.appendChar(';')
      else buffer.queue(';')
      generate(tail, Some(Semicolon(force)))
    case Space(force) :: tail =>
      if (force) space()
      else if (buffer.hasContent) {
        val last = buffer.getLastChar
        if (last != ' ' && last != '\n') space()
      }
      generate(tail, Some(Space(force)))
    case Word(str) :: tail =>
      lastCmd match {
        case Some(w: Word) => space()
        case Some(n: Number) => space()
        case _ if (str.startsWith("/") && buffer.getLastChar == '/') => space()
        case _ => ()
      }
      buffer.append(str, false)
      generate(tail, Some(Word(str)))
    case Number(str) :: tail =>
      lastCmd match {
        case Some(w: Word) => space()
        case _ => ()
      }
      buffer.append(str, false)
      generate(tail, Some(Number(str)))
    case Token(str, maybeNewline) :: tail =>
      lastCmd match {
        case Some(n: Number) if (str.startsWith(".")) => space()
        case _ if (buffer.getLastChar == '!' && str.startsWith("--") ||
          buffer.getLastChar == '/' && str.startsWith("/") ||
          buffer.getLastChar == '+' && str.startsWith("+")) => space()
        case _ => ()
      }
      buffer.append(str, maybeNewline)
      generate(tail, Some(Token(str, maybeNewline)))
    case Newline(i, force) :: tail =>
      if (i > 0 && force) {
        val exactNewline = (if (i > 2) 2 else i) - buffer.getNewlineCount
        if (i == 2) { buffer.queue('\n'); buffer.queue('\n') }
        else if (i == 1) buffer.queue('\n')
      }
      generate(tail, Some(Newline(i, force)))
    case Nil => ()
  }

  private def space() = buffer.queue(' ')

  def get = buffer.get()

  // private def printStatementAfterKeyword(node: Option[Node], parent: Node, isLabel: Boolean)(implicit options: PrinterOptions) =
  //   node match {
  //     case Some(node) =>
  //       space()
  //       printTerminatorless(node, parent, isLabel)
  //       semicolon()
  //     case _ => semicolon()
  //   }

  // private def tsPrintBraced(members: List[Node], node: Node)(implicit options: PrinterOptions) = {
  //   token("{")
  //   if (members.length > 0) {
  //     indent()
  //     newline()
  //     members.foreach((m) => {
  //       print(Some(m), Some(node))
  //       newline()
  //     })
  //     dedent()
  //   }

  //   sourceWithOffset(LocationType.End, node.location, 0, -1)
  //   rightBrace()
  // }

  
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

  // private def tsPrintUnionOrIntersectionType(types: List[Node], node: Node, sep: String)(implicit options: PrinterOptions) =
  //   printJoin(Some(types), node, PrintSequenceOptions(
  //     separator = Some((p: Printer) => { p.space(); p.token(sep); p.space(); })
  //   ))

  // private def param(parameter: Identifier | RestElement | Node with Pattern | TSParameterProperty, parent: Option[Node] = None)(implicit options: PrinterOptions) = {
  //   val dec = parameter match {
  //     case id: Identifier => id.decorators
  //     case e: RestElement => e.decorators
  //     // TODO: move decorators to Pattern trait
  //     case p: TSParameterProperty => p.decorators
  //     case _ => None
  //   }

  //   printJoin(dec, parameter, PrintSequenceOptions())
  //   print(Some(parameter), parent)
  // }

end CodeGenerator

object CodeGenerator:
  def apply(format: Format, sourceMap: SourceMapBuilder) =
    new CodeGenerator(format, sourceMap)
  // def isFor(node: Node): Boolean =
  //   node match {
  //     case _: ForInStatement => true
  //     case _: ForOfStatement => true
  //     case _: ForStatement => true
  //     case _ => false
  //   }
