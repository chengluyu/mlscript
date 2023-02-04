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
    case (semi: Semicolon) :: tail =>
      queueIndentation(semi.indentLevel)
      if (semi.force) buffer.appendChar(';')
      else buffer.queue(';')
      generate(tail, Some(semi))
    case (sp: Space) :: tail =>
      queueIndentation(sp.indentLevel)
      if (sp.force) space()
      else if (buffer.hasContent) {
        val last = buffer.getLastChar
        if (last != ' ' && last != '\n') space()
      }
      generate(tail, Some(sp))
    case (wd: Word) :: tail =>
      queueIndentation(wd.indentLevel)
      lastCmd match {
        case Some(w: Word) => space()
        case Some(n: Number) => space()
        case _ if (wd.str.startsWith("/") && buffer.getLastChar == '/') => space()
        case _ => ()
      }
      buffer.append(wd.str, false)
      generate(tail, Some(wd))
    case (num: Number) :: tail =>
      queueIndentation(num.indentLevel)
      lastCmd match {
        case Some(w: Word) => space()
        case _ => ()
      }
      buffer.append(num.str, false)
      generate(tail, Some(num))
    case (tk: Token) :: tail =>
      queueIndentation(tk.indentLevel)
      lastCmd match {
        case Some(n: Number) if (tk.str.startsWith(".")) => space()
        case _ if (buffer.getLastChar == '!' && tk.str.startsWith("--") ||
          buffer.getLastChar == '/' && tk.str.startsWith("/") ||
          buffer.getLastChar == '+' && tk.str.startsWith("+")) => space()
        case _ => ()
      }
      buffer.append(tk.str, tk.maybeNewline)
      generate(tail, Some(tk))
    case (nl: Newline) :: tail =>
      if (nl.i > 0 && buffer.hasContent) {
        val exactNewline = (if (nl.i > 2) 2 else nl.i) - buffer.getNewlineCount
        if (exactNewline == 2) { queueIndentation(nl.indentLevel); buffer.queue('\n'); queueIndentation(nl.indentLevel); buffer.queue('\n') }
        else if (exactNewline == 1) { queueIndentation(nl.indentLevel); buffer.queue('\n') }
      }

      generate(tail, Some(nl))
    case Nil => ()
  }

  private def space() = buffer.queue(' ')
  private def queueIndentation(repeat: Int) =
    if (repeat > 0 && buffer.getLastChar == '\n') buffer.queueIndentation(' ', repeat * 2) // TODO: add settings

  def get = buffer.get()
end CodeGenerator

object CodeGenerator:
  def apply(format: Format, sourceMap: SourceMapBuilder) =
    new CodeGenerator(format, sourceMap)
