package mlscript.codegen.generator

import mlscript.codegen.LocationType
import mlscript.codegen.ast._
import mlscript.codegen.LocationType
import mlscript.codegen.generator._

enum PrintType:
  case Empty
  case Semicolon
  case Space
  case Word
  case Number
  case Token
  case Newline

abstract class Printer(
  format: Format,
  sourceMap: SourceMapBuilder
):
  private val buffer = new Buffer(Some(sourceMap))

  def print(
    node: Node,
    parent: Option[Node] = None,
    previous: PrintType = PrintType.Empty,
    forceParens: Boolean = false
  )(implicit indentLevel: Int, stack: List[Node] = List()): PrintType = ???

  // We don't actually use `previous` in this function, but it helps us chain everything together
  protected def printSemicolon(previous: PrintType)(implicit indentLevel: Int = 0): PrintType = {
    queueIndentation(indentLevel)
    buffer.appendChar(';')
    PrintType.Semicolon
  }

  protected def printSpace(previous: PrintType)(implicit indentLevel: Int = 0): PrintType = {
    queueIndentation(indentLevel)
    val last = buffer.getLastChar
    if (last != ' ' && last != '\n') {
      space()
      PrintType.Space
    }
    else previous
  }

  protected def printWord(str: String, previous: PrintType)(implicit indentLevel: Int = 0): PrintType = {
    queueIndentation(indentLevel)
    previous match {
      case PrintType.Word => space()
      case PrintType.Number => space()
      case _ if (str.startsWith("/") && buffer.getLastChar == '/') => space()
      case _ => ()
    }
    buffer.append(str, false)
    PrintType.Word
  }

  protected def printNumber(str: String, previous: PrintType)(implicit indentLevel: Int = 0): PrintType = {
    queueIndentation(indentLevel)
    previous match {
      case PrintType.Word => space()
      case _ => ()
    }
    buffer.append(str, false)
    PrintType.Number
  }

  protected def printToken(
    str: String,
    previous: PrintType,
    maybeNewline: Boolean = false
  )(implicit indentLevel: Int = 0): PrintType = {
    queueIndentation(indentLevel)
    previous match {
      case PrintType.Number if (str.startsWith(".")) => space()
      case _ if (buffer.getLastChar == '!' && str.startsWith("--") ||
        buffer.getLastChar == '/' && str.startsWith("/") ||
        buffer.getLastChar == '+' && str.startsWith("+")) => space()
      case _ => ()
    }
    buffer.append(str, maybeNewline)
    PrintType.Token
  }

  protected def printNewline(i: Int, previous: PrintType)(implicit indentLevel: Int = 0): PrintType =
    if (i > 0 && buffer.hasContent) {
      val exactNewline = (if (i > 2) 2 else i) - buffer.getNewlineCount
      if (exactNewline == 2) { queueIndentation(indentLevel); buffer.queue('\n'); queueIndentation(indentLevel); buffer.queue('\n') }
      else if (exactNewline == 1) { queueIndentation(indentLevel); buffer.queue('\n') }
      PrintType.Newline
    }
    else previous

  protected def printList(lst: List[PrintCommand], previous: PrintType)(implicit indentLevel: Int = 0): PrintType = lst match {
    case (semi: Semicolon) :: tail =>
      printList(tail, printSemicolon(previous))
    case (sp: Space) :: tail =>
      printList(tail, printSpace(previous))
    case (wd: Word) :: tail =>
      printList(tail, printWord(wd.str, previous))
    case (num: Number) :: tail =>
      printList(tail, printNumber(num.str, previous))
    case (tk: Token) :: tail =>
      printList(tail, printToken(tk.str, previous, tk.maybeNewline))
    case (nl: Newline) :: tail =>
      printList(tail, printNewline(nl.i, previous))
    case Nil => previous
  }

  private def space() = buffer.queue(' ')
  private def queueIndentation(repeat: Int) =
    if (repeat > 0 && buffer.getLastChar == '\n') buffer.queueIndentation(' ', repeat * 2) // TODO: add settings

  def get = buffer.get()
end Printer
