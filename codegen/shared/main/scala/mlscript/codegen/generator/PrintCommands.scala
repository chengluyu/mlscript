package mlscript.codegen.generator

import mlscript.codegen.{Location => SourceLocation}

sealed abstract class PrintCommand()(implicit indentLevel: Int)

final case class Semicolon(
  force: Boolean
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Space(
  force: Boolean,
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Word(
  str: String
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Number(
  str: String
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Token(
  str: String,
  maybeNewline: Boolean
)(implicit indentLevel: Int = 0) extends PrintCommand

final case class Newline(
  i: Int,
  force: Boolean
)(implicit indentLevel: Int = 0) extends PrintCommand
