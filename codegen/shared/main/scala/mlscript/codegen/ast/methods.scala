package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

case class FunctionExpression(
  val id: Option[Identifier] = None,
  val params: List[Identifier | Node with Pattern | RestElement],
  val body: BlockStatement,
  val generator: Boolean = false,
  val async: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Function with BlockParent with FunctionParent with Expression with Pureish:
  var returnType: Option[TSTypeAnnotation] = None
  var typeParameters: Option[TSTypeParameterDeclaration] = None

case class FunctionDeclaration(
  val id: Option[Identifier] = None,
  val params: List[Identifier | Node with Pattern | RestElement],
  val body: BlockStatement,
  val generator: Boolean = false,
  val async: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Function with BlockParent with FunctionParent with Statement with Pureish with Declaration:
  var declare: Option[Boolean] = None
  var returnType: Option[TSTypeAnnotation] = None
  var typeParameters: Option[TSTypeParameterDeclaration] = None

case class ArrowFunctionExpression(
  val params: List[Identifier | Node with Pattern | RestElement],
  val body: BlockStatement | Node with Expression,
  val async: Boolean = false,
  val expression: Boolean
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Function with BlockParent with FunctionParent with Expression with Pureish:
  var generator: Option[Boolean] = None
  var returnType: Option[TSTypeAnnotation] = None
  var typeParameters: Option[TSTypeParameterDeclaration] = None
