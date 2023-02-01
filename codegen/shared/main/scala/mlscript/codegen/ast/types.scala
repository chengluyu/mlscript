package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

case class Identifier(val name: String)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Expression with PatternLike with LVal with TSEntityName:
  var decorators: Option[List[Decorator]] = None
  var optional: Option[Boolean] = None
  var typeAnnotation: Option[TSTypeAnnotation | Noop] = None

case class ArgumentPlaceholder()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node

case class RestElement(val argument: Node with LVal)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with LVal with PatternLike:
  var decorators: Option[List[Decorator]] = None
  var optional: Option[Boolean] = None
  var typeAnnotation: Option[TSTypeAnnotation | Noop] = None

case class SpreadElement(val argument: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with UnaryLike

case class ObjectExpression(val properties: List[ObjectMethod | ObjectProperty | SpreadElement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression

case class ObjectPattern(val properties: List[RestElement | ObjectProperty])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Pattern with PatternLike with LVal:
  var decorators: Option[List[Decorator]] = None
  var typeAnnotation: Option[TSTypeAnnotation | Noop] = None

enum ObjectMethodKind:
  case Method
  case Getter
  case Setter
  case Init

case class ObjectMethod(
  val kind: Option[ObjectMethodKind] = Some(ObjectMethodKind.Method),
  val key: Node with Expression | Identifier | StringLiteral | NumericLiteral | BigIntLiteral,
  val params: List[Identifier | Node with Pattern | RestElement],
  val body: BlockStatement,
  val computed: Boolean = false,
  val generator: Boolean = false,
  val async: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with UserWhitespacable with Function with Scopable with BlockParent with FunctionParent with Method with ObjectMember:
  var decorators: Option[List[Decorator]] = None
  var returnType: Option[TSTypeAnnotation | Noop] = None
  var typeParameters: Option[TSTypeParameterDeclaration | Noop] = None

case class ObjectProperty(
  val key: Node with Expression | Identifier | StringLiteral | NumericLiteral | BigIntLiteral | DecimalLiteral | PrivateName,
  val value: Node with Expression | Node with PatternLike,
  val computed: Boolean = false,
  val shorthand: Boolean = false,
  val decorators: Option[List[Decorator]] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with UserWhitespacable with Property with ObjectMember

case class ArrayExpression(val elements: List[Option[(Node with Expression) | (Node with SpreadElement)]] = Nil)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression

case class ArrayPattern(val elements: List[Option[Node with PatternLike | Node with LVal]])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Pattern with PatternLike with LVal:
  var decorators: Option[List[Decorator]] = None
  var optional: Option[Boolean] = None
  var typeAnnotation: Option[TSTypeAnnotation | Noop] = None

case class RecordExpression(val properties: List[ObjectProperty | SpreadElement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

case class TupleExpression(val elements: List[Node with Expression | SpreadElement] = Nil)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
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

case class PipelineTopicExpression(val expression: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

case class PipelineBareFunction(val callee: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

case class PipelinePrimaryTopicReference()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression
