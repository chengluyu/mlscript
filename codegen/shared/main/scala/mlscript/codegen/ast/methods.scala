package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

case class FunctionExpression(
  id: Option[Identifier] = None,
  params: List[Identifier | Node with Pattern | RestElement],
  body: BlockStatement,
  generator: Boolean = false,
  async: Boolean = false,
  returnType: Option[TSTypeAnnotation] = None,
  typeParameters: Option[TSTypeParameterDeclaration] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Function with BlockParent with FunctionParent with Expression with Pureish

case class FunctionDeclaration(
  id: Option[Identifier] = None,
  params: List[Identifier | Node with Pattern | RestElement],
  body: BlockStatement,
  generator: Boolean = false,
  async: Boolean = false,
  declare: Boolean = false,
  returnType: Option[TSTypeAnnotation] = None,
  typeParameters: Option[TSTypeParameterDeclaration] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Function with BlockParent with FunctionParent with Statement with Pureish with Declaration

case class ArrowFunctionExpression(
  params: List[Identifier | Node with Pattern | RestElement],
  body: BlockStatement | Node with Expression,
  async: Boolean = false,
  expression: Boolean,
  generator: Boolean = false,
  returnType: Option[TSTypeAnnotation] = None,
  typeParameters: Option[TSTypeParameterDeclaration] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Function with BlockParent with FunctionParent with Expression with Pureish  
