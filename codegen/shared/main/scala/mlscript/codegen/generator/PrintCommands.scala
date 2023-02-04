package mlscript.codegen.generator

import mlscript.codegen.{Location => SourceLocation}

sealed abstract class PrintCommand()(implicit indentLevel: Int)

final case class Semicolon(
  val force: Boolean = false
)(implicit val indentLevel: Int = 0) extends PrintCommand

final case class Space(
  val force: Boolean = false,
)(implicit val indentLevel: Int = 0) extends PrintCommand

final case class Word(
  val str: String
)(implicit val indentLevel: Int = 0) extends PrintCommand

final case class Number(
  val str: String
)(implicit val indentLevel: Int = 0) extends PrintCommand

final case class Token(
  val str: String,
  val maybeNewline: Boolean = false
)(implicit val indentLevel: Int = 0) extends PrintCommand

final case class Newline(
  val i: Int = 1,
  val force: Boolean = false
)(implicit val indentLevel: Int = 0) extends PrintCommand
