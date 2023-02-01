package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

case class WithStatement(val `object`: Node with Expression, val body: Node with Statement)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement

case class IfStatement(
  val test: Node with Expression,
  val consequent: Node with Statement,
  val alternate: Option[Node with Statement] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Conditional

case class ForStatement(
  val init: Option[VariableDeclaration | Node with Expression] = None,
  val test: Option[Node with Expression] = None,
  val update: Option[Node with Expression] = None,
  val body: Node with Statement
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Statement with For with BlockParent with Loop

case class WhileStatement(val test: Node with Expression, val body: Node with Statement)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with BlockParent with Loop with While with Scopable

case class ForInStatement(
  val left: VariableDeclaration | Node with LVal,
  val right: Node with Expression,
  val body: Node with Statement
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Statement with For with BlockParent with Loop with ForXStatement

case class ForOfStatement(
  val left: VariableDeclaration | Node with LVal,
  val right: Node with Expression,
  val body: Node with Statement,
  val await: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Statement with For with BlockParent with Loop with ForXStatement

case class DoWhileStatement(val test: Node with Expression, val body: Node with Statement)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with BlockParent with Loop with While with Scopable

case class BreakStatement(val label: Option[Identifier] = None)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with Terminatorless with CompletionStatement

case class ContinueStatement(val label: Option[Identifier] = None)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with Terminatorless with CompletionStatement

case class ReturnStatement(val argument: Option[Node with Expression] = None)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with Terminatorless with CompletionStatement

case class ThrowStatement(val argument: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with Terminatorless with CompletionStatement

case class LabeledStatement(val label: Identifier, val body: Node with Statement)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement

case class TryStatement(
  val block: BlockStatement,
  val handler: Option[CatchClause] = None,
  val finalizer: Option[BlockStatement] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement

case class CatchClause(
  val param: Option[Identifier | ArrayPattern | ObjectPattern] = None,
  val body: BlockStatement
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with BlockParent

case class SwitchCase(
  val test: Option[Node with Expression] = None,
  val consequent: List[Node with Statement]
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized

case class SwitchStatement(val discriminant: Node with Expression, val cases: List[SwitchCase])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with BlockParent with Scopable

case class DebuggerStatement()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement

enum VariableDeclarationKind:
  case Var
  case Let
  case Const
  case Using

case class VariableDeclaration(
  val kind: VariableDeclarationKind,
  val declarations: List[VariableDeclarator]
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Declaration:
  var declare: Option[Boolean] = None

case class VariableDeclarator(val id: Node with LVal, val init: Option[Node with Expression] = None)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized:
  var definite: Option[Boolean] = None
