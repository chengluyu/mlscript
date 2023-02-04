package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

case class Identifier(name: String, val typeAnnotation: Option[TSTypeAnnotation] = None)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Expression with PatternLike with LVal with TSEntityName:
  var decorators: Option[List[Decorator]] = None
  var optional: Option[Boolean] = None

case class ArgumentPlaceholder()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node

case class RestElement(
  argument: Node with LVal,
  typeAnnotation: Option[TSTypeAnnotation] = None,
  decorators: List[Decorator] = List(),
  optional: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with LVal with PatternLike

case class SpreadElement(val argument: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with UnaryLike

case class ObjectExpression(properties: List[ObjectMethod | ObjectProperty | SpreadElement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression

case class ObjectPattern(
  properties: List[RestElement | ObjectProperty],
  typeAnnotation: Option[TSTypeAnnotation] = None,
  decorators: List[Decorator] = List()
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Pattern with PatternLike with LVal

enum ObjectMethodKind:
  case Method
  case Getter
  case Setter
  case Init

case class ObjectMethod(
  val kind: ObjectMethodKind = ObjectMethodKind.Method,
  val key: Node with Expression | Identifier | StringLiteral | NumericLiteral | BigIntLiteral,
  val params: List[Identifier | Node with Pattern | RestElement],
  val body: BlockStatement,
  val computed: Boolean = false,
  val generator: Boolean = false,
  val async: Boolean = false,
  var decorators: List[Decorator] = Nil,
  var returnType: Option[TSTypeAnnotation] = None,
  var typeParameters: Option[TSTypeParameterDeclaration] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with UserWhitespacable with Function with Scopable with BlockParent with FunctionParent with Method with ObjectMember

case class ObjectProperty(
  key: Node with Expression | Identifier | StringLiteral | NumericLiteral | BigIntLiteral | DecimalLiteral | PrivateName,
  value: Node with Expression | Node with PatternLike,
  computed: Boolean = false,
  shorthand: Boolean = false,
  decorators: List[Decorator] = Nil
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with UserWhitespacable with Property with ObjectMember

case class ArrayExpression(elements: List[Option[(Node with Expression) | (Node with SpreadElement)]] = Nil)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression

case class ArrayPattern(
  elements: List[Option[Node with PatternLike | Node with LVal]],
  typeAnnotation: Option[TSTypeAnnotation] = None,
  decorators: List[Decorator] = List(),
  optional: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Pattern with PatternLike with LVal

case class RecordExpression(properties: List[ObjectProperty | SpreadElement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

case class TupleExpression(elements: List[Node with Expression | SpreadElement] = Nil)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

case class RegExpLiteral(val pattern: String, val flags: String = "")(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression with Pureish with Literal

case class StringLiteral(val value: String)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression with Pureish with Literal with Immutable

case class NumericLiteral(val value: Int | Double)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression with Pureish with Literal with Immutable

case class NullLiteral()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression with Pureish with Literal with Immutable

case class BooleanLiteral(val value: Boolean)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression with Pureish with Literal with Immutable

case class BigIntLiteral(val value: String)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression with Pureish with Literal with Immutable

case class DecimalLiteral(val value: String)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression with Pureish with Literal with Immutable

case class TopicReference()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

case class PipelineTopicExpression(expression: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

case class PipelineBareFunction(callee: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

case class PipelinePrimaryTopicReference()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression
