package mlscript.codegen.generator

import mlscript.codegen.{Location => SourceLocation}

sealed abstract class PrintCommand()(implicit indentLevel: Int)

final case class Semicolon(
  force: Boolean = false
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Space(
  force: Boolean = false,
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Word(
  str: String
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Number(
  str: String
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Token(
  str: String,
  maybeNewline: Boolean = false
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Newline(
  i: Int = 1,
  force: Boolean = false
)(implicit indentLevel: Int = 0) extends PrintCommand
