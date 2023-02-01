package mlscript.codegen.generator

import mlscript.codegen.{Location => SourceLocation}

sealed abstract class PrintCommand(
  start: Option[Int],
  end: Option[Int],
  location: Option[SourceLocation]
)(implicit indentLevel: Int)

final case class Semicolon(
  force: Boolean,
  start: Option[Int],
  end: Option[Int],
  location: Option[SourceLocation]
)(implicit indentLevel: Int) extends PrintCommand(start, end, location)

final case class Space(
  force: Boolean,
  start: Option[Int],
  end: Option[Int],
  location: Option[SourceLocation]
)(implicit indentLevel: Int) extends PrintCommand(start, end, location)

final case class Word(
  str: String,
  start: Option[Int],
  end: Option[Int],
  location: Option[SourceLocation]
)(implicit indentLevel: Int) extends PrintCommand(start, end, location)

final case class Number(
  str: String,
  start: Option[Int],
  end: Option[Int],
  location: Option[SourceLocation]
)(implicit indentLevel: Int) extends PrintCommand(start, end, location)

final case class Token(
  str: String,
  maybeNewline: Boolean,
  start: Option[Int],
  end: Option[Int],
  location: Option[SourceLocation]
)(implicit indentLevel: Int) extends PrintCommand(start, end, location)

final case class Newline(
  i: Int,
  force: Boolean,
  start: Option[Int],
  end: Option[Int],
  location: Option[SourceLocation]
)(implicit indentLevel: Int) extends PrintCommand(start, end, location)
