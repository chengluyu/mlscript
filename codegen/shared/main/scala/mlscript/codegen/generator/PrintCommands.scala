package mlscript.codegen.generator

import mlscript.codegen.{Location => SourceLocation}

sealed abstract class PrintCommand()

final case class Semicolon() extends PrintCommand
final case class Space() extends PrintCommand
final case class Word(val str: String) extends PrintCommand
final case class Number(val str: String) extends PrintCommand
final case class Token(
  val str: String,
  val maybeNewline: Boolean = false
) extends PrintCommand
final case class Newline(val i: Int = 1) extends PrintCommand
