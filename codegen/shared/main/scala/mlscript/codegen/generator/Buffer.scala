package mlscript.codegen.generator

import scala.collection.mutable.{StringBuilder, ArrayDeque}
import mlscript.codegen.{Position, Location, LocationType}
import mlscript.codegen.ast.Node

private case class QueueItem(
  val char: Char,
  val repeat: Int,
  val sourcePosition: SourcePosition
)

// TODO: Find a better name.
class BufferOutput(
  val code: String,
  val decodeMap: Option[Any], // TODO: Fill the right types after they're done.
  val map: Option[SourceMapBuilder],
  val rawMappings: Option[Any] // TODO: Fill the right types after they're done.
)

class Buffer(map: Option[SourceMapBuilder]) {
  private var position: Position = Position(1, 0)

  private var sourcePosition: SourcePosition = SourcePosition()

  private var buf = new StringBuilder() // Contents in buffer are not revertable
  // TODO: The queue may be removed.
  private var revertableQueue = new ArrayDeque[QueueItem](16) // No need for _allocQueue. We can initialize it by using constructor.

  private def last: Char =
    if (buf.isEmpty) '\u0000' else buf.last

  private def pushQueue(
    char: Char,
    repeat: Int,
    line: Option[Int],
    column: Option[Int],
    identifierName: Option[String],
    fileName: Option[String]
  ): Unit = 
    revertableQueue = revertableQueue :+ QueueItem(char, repeat, SourcePosition(identifierName, line, column, fileName))

  /**
   * Get the final string output from the buffer, along with the sourcemap if one exists.
   */
  def get(): BufferOutput = {
    flush()
    val code = buf.toString().reverse.dropWhile(_.isWhitespace).reverse
    new BufferOutput(code, None, None, None)
  }

   /**
   * Add a string to the buffer that cannot be reverted.
   */
  def appendChar(char: Char): Unit = {
    flush()
    appendChar(char, 1, sourcePosition)
  }

  /**
   * Add a string to the buffer than can be reverted.
   */
  def queue(char: Char): Unit = {
    // Drop trailing spaces when a newline is inserted.
    if (char == '\n') {
      revertableQueue.removeLastWhile(
        (item) => item.char == ' ' || item.char == '\t'
      )
    }

    pushQueue(char, 1,
      sourcePosition.line,
      sourcePosition.column,
      sourcePosition.identifierName,
      sourcePosition.fileName)
  }

  /**
   * Same as queue, but this indentation will never have a sourcmap marker.
   */
  def queueIndentation(char: Char, repeat: Int): Unit =
    pushQueue(char, repeat, None, None, None, None)

  private def flush(): Unit = {
    revertableQueue.foreach(
      (item) => appendChar(item.char, item.repeat, item.sourcePosition))
    revertableQueue.clear()
  }

  private def appendChar(
    char: Char,
    repeat: Int,
    sourcePos: SourcePosition
  ): Unit = {
    buf =
      if (repeat > 1) buf.append(char.toString() * repeat)
      else buf.append(char)

    if (char != '\n') {
      // Why not `mark(sourcePos)`?
      mark(
        sourcePos.line,
        sourcePos.column,
        sourcePos.identifierName,
        sourcePos.fileName
      )
      position = position + repeat
    } else {
      position = position.nextLine
    }
  }

   /**
   * Add a string to the buffer that cannot be reverted.
   */
  def append(str: String, maybeNewline: Boolean): Unit = {
    flush()
    buf = buf.append(str)

    if (!maybeNewline && map.isEmpty)
      position = position + str.length
    else {
      var lastPos = 0

      // If the string starts with a newline char, then adding a mark is redundant.
      // This catches both "no newlines" and "newline after several chars".
      if (!str.startsWith("\n"))
        mark(sourcePosition.line, sourcePosition.column, sourcePosition.identifierName, sourcePosition.fileName)

      str.split("\n").zipWithIndex.foreach((ls, i) => {
        position = position.nextLine
        lastPos += ls.length()

        if (lastPos < str.length) {
          mark(Some(sourcePosition.line.get + i + 1), Some(0), sourcePosition.identifierName, sourcePosition.fileName)
        }
      })

      position = position + (str.length - lastPos)
    }
  }

  private def mark(
    line: Option[Int],
    column: Option[Int],
    identifierName: Option[String],
    fileName: Option[String]
  ): Unit = if (map.isDefined && line.isDefined && column.isDefined && identifierName.isDefined)
      map.get.mark(position, Position(line.get, column.get),
        fileName.getOrElse(""), identifierName)

  def removeTrailingNewline(): Unit =
    if (!revertableQueue.isEmpty && revertableQueue.last.char == '\n')
      revertableQueue.dropRightInPlace(1)
  
  def removeLastSemicolon(): Unit =
    if (!revertableQueue.isEmpty && revertableQueue.last.char == ';')
      revertableQueue.dropRightInPlace(1)

  def getLastChar: Char =
    if (!revertableQueue.isEmpty) revertableQueue.last.char else last
  
  def getNewlineCount: Int = 
    if (revertableQueue.isEmpty) {
      if (last == '\n') 1 else 0
    }
    else {
      val count = revertableQueue.foldRight((0, true))((item, p) =>
        if (item.char == '\n' && p._2) (p._1 + 1, true)
        else (p._1, false))._1
      if (count == revertableQueue.length && last == '\n')
        count + 1
      else count
    }

  def endsWithCharAndNewline: Option[Char] =
    if (!revertableQueue.isEmpty) {
      val lastCp = revertableQueue.last
      if (lastCp.char != '\n') None
      else if (revertableQueue.length > 1)
        Some(revertableQueue(revertableQueue.length - 2).char)
      else None
    }
    else None

  def hasContent: Boolean =
    !revertableQueue.isEmpty || last != '\u0000'

  // def exactSource(loc: Option[Location], node: Node, parent: Option[Node], printer: Printer)(implicit options: PrinterOptions): Unit =
  //   if (map.isEmpty) printer.print(node, parent)
  //   else {
  //     source(LocationType.Start, loc)
  //     printer.print(node, parent)
  //     source(LocationType.End, loc)
  //   }

  def source(prop: LocationType, loc: Option[Location]): Unit =
    if (!map.isEmpty) normalizePosition(prop, loc, 0, 0)

  def sourceWithOffset(
    prop: LocationType,
    loc: Option[Location],
    lineOffset: Int,
    columnOffset: Int
  ): Unit =
    if (!map.isEmpty) normalizePosition(prop, loc, lineOffset, columnOffset)

  // def withSource(prop: LocationType, loc: Option[Location], node: Node, parent: Node, printer: Printer)(implicit options: PrinterOptions): Unit =
  //   if (map.isEmpty) printer.print(node, Some(parent))
  //   else {
  //     source(prop, loc)
  //     printer.print(node, Some(parent))
  //   }

  private def normalizePosition(
    prop: LocationType,
    loc: Option[Location],
    lineOffset: Int,
    columnOffset: Int
  ): Unit = {
    val pos = loc.flatMap(_(prop))
    val targetIdName: Option[String] = prop match
      case LocationType.Start => loc.flatMap(_.identifierName)
      case LocationType.End => None
    sourcePosition = pos match
      case Some(pos) => SourcePosition(targetIdName, Some(pos.line + lineOffset), Some(pos.column + columnOffset), loc.flatMap(_.fileName))
      case None => SourcePosition(targetIdName, sourcePosition.line, sourcePosition.column, sourcePosition.fileName)
  }

  def getCurrentColumn: Int = {
    var lastIndex = -1
    val len = revertableQueue.foldLeft(0)((le, item) => {
      if (item.char == '\n') lastIndex = le
      le + item.repeat
    })

    if (lastIndex == -1) position.column + len
    else len - 1 - lastIndex
  }

  def getCurrentLine: Int =
    position.line + revertableQueue.foldLeft(0)((count, item) =>
      if (item.char == '\n') count + 1 else count)
}
