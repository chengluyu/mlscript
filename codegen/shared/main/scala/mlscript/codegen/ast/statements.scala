package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

case class WithStatement(`object`: Node with Expression, body: Node with Statement)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement

case class IfStatement(
  test: Node with Expression,
  consequent: Node with Statement,
  alternate: Option[Node with Statement] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Conditional

case class ForStatement(
  init: Option[VariableDeclaration | Node with Expression] = None,
  test: Option[Node with Expression] = None,
  update: Option[Node with Expression] = None,
  body: Node with Statement
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Statement with For with BlockParent with Loop

case class WhileStatement(test: Node with Expression, body: Node with Statement)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with BlockParent with Loop with While with Scopable

case class ForInStatement(
  left: VariableDeclaration | Node with LVal,
  right: Node with Expression,
  body: Node with Statement
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Statement with For with BlockParent with Loop with ForXStatement

case class ForOfStatement(
  left: VariableDeclaration | Node with LVal,
  right: Node with Expression,
  body: Node with Statement,
  await: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Statement with For with BlockParent with Loop with ForXStatement

case class DoWhileStatement(test: Node with Expression, body: Node with Statement)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with BlockParent with Loop with While with Scopable

case class BreakStatement(val label: Option[Identifier] = None)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with Terminatorless with CompletionStatement

case class ContinueStatement(val label: Option[Identifier] = None)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with Terminatorless with CompletionStatement

case class ReturnStatement(val argument: Option[Node with Expression] = None)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with Terminatorless with CompletionStatement

case class ThrowStatement(val argument: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with Terminatorless with CompletionStatement

case class LabeledStatement(label: Identifier, body: Node with Statement)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement

case class TryStatement(
  block: BlockStatement,
  handler: Option[CatchClause] = None,
  finalizer: Option[BlockStatement] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement

case class CatchClause(
  param: Option[Identifier | ArrayPattern | ObjectPattern] = None,
  body: BlockStatement
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with BlockParent

case class SwitchCase(
  test: Option[Node with Expression] = None,
  consequent: List[Node with Statement]
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized

case class SwitchStatement(discriminant: Node with Expression, cases: List[SwitchCase])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with BlockParent with Scopable

case class DebuggerStatement()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement

enum VariableDeclarationKind:
  case Var
  case Let
  case Const
  case Using

case class VariableDeclaration(
  kind: VariableDeclarationKind,
  declarations: List[VariableDeclarator],
  declare: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Statement with Declaration

case class VariableDeclarator(id: Node with LVal, val init: Option[Node with Expression] = None, definite: Boolean = false)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized
