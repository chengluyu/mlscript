package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

enum UnaryOperator:
  case Plus
  case Negation
  case LogicalNot
  case BitwiseNot
  case TypeOf
  case Void
  case Delete
  case Throw

case class UnaryExpression(
  val operator: UnaryOperator,
  val argument: Node with Expression,
  val prefix: Boolean = true
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with UnaryLike with Expression

case class DoExpression(body: BlockStatement, async: Boolean = false)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

case class ParenthesizedExpression(expression: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression with ExpressionWrapper

enum UpdateOperator:
  case Increment
  case Decrement

object UpdateOperator {
  def to(op: UpdateOperator): String = op match {
    case Increment => "++"
    case Decrement => "--"
  }
}

case class UpdateExpression(
  operator: UpdateOperator,
  argument: Node with Expression,
  prefix: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression

case class ConditionalExpression(
  test: Node with Expression,
  consequent: Node with Expression,
  alternate: Node with Expression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression with Conditional

case class NewExpression(
  callee: Node with Expression | Super | V8IntrinsicIdentifier,
  arguments: List[Node with Expression | SpreadElement | ArgumentPlaceholder],
  optional: Boolean = false,
  typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression

case class SequenceExpression(expressions: List[Node with Expression])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression

case class ThisExpression()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression

case class Super()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression

case class Decorator(expression: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node

case class OptionalMemberExpression(
  `object`: Node with Expression,
  property: Node with Expression | Identifier,
  computed: Boolean = false,
  optional: Boolean
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression

case class OptionalCallExpression(
  callee: Node with Expression,
  arguments: List[Node with Expression | SpreadElement | ArgumentPlaceholder],
  optional: Boolean,
  typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression

case class CallExpression(
  callee: Node with Expression | Super | V8IntrinsicIdentifier,
  arguments: List[Node with Expression | SpreadElement | ArgumentPlaceholder],
  optional: Boolean = false,
  typeParameters: Option[TSTypeParameterInstantiation] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression

case class Import()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression

case class AwaitExpression(argument: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression with Terminatorless

case class YieldExpression(
  argument: Option[Node with Expression] = None,
  delegate: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression with Terminatorless

case class EmptyStatement()(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Statement

case class ExpressionStatement(expression: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Statement with ExpressionWrapper

case class AssignmentPattern(
  left: Identifier | ObjectPattern | ArrayPattern | MemberExpression | TSAsExpression | TSSatisfiesExpression | TSTypeAssertion | TSNonNullExpression,
  right: Node with Expression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Pattern with PatternLike with LVal

case class AssignmentExpression(
  operator: String,
  left: Node with LVal,
  right: Node with Expression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression

case class BindExpression(`object`: Node with Expression, callee: Node with Expression)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression

enum BinaryOperator:
  case Plus
  case Minus
  case Divide
  case Modolus
  case Multiplication
  case Exponentiation
  case BitwiseAnd
  case BitwiseOr
  case BitwiseRightShift
  case BitwiseUnsignedRightShift
  case BitwiseLeftShift
  case BitwiseXor
  case Equal
  case StrictEqual
  case NotEqual
  case StrictNotEqual
  case In
  case InstanceOf
  case GreaterThan
  case LessThan
  case GreaterThanOrEqual
  case LessThanOrEqual
  case Pipeline

object BinaryOperator:
  def from(op: String): Option[BinaryOperator] =
    op match
      case "+" => Some(Plus)
      case "-" => Some(Minus)
      case "/" => Some(Divide)
      case "%" => Some(Modolus)
      case "*" => Some(Multiplication)
      case "**" => Some(Exponentiation)
      case "&" => Some(BitwiseAnd)
      case "|" => Some(BitwiseOr)
      case ">>" => Some(BitwiseRightShift)
      case ">>>" => Some(BitwiseUnsignedRightShift)
      case "<<" => Some(BitwiseLeftShift)
      case "^" => Some(BitwiseXor)
      case "==" => Some(Equal)
      case "===" => Some(StrictEqual)
      case "!=" => Some(NotEqual)
      case "!==" => Some(StrictNotEqual)
      case "in" => Some(In)
      case "instanceof" => Some(InstanceOf)
      case ">" => Some(GreaterThan)
      case "<" => Some(LessThan)
      case ">=" => Some(GreaterThanOrEqual)
      case "<=" => Some(LessThanOrEqual)
      case "|>" => Some(Pipeline)
      case _ => None
  def to(op: BinaryOperator): String = op match {
    case Plus => "+"
    case Minus => "-"
    case Divide => "/"
    case Modolus => "%"
    case Multiplication => "*"
    case Exponentiation => "**"
    case BitwiseAnd => "&"
    case BitwiseOr => "|"
    case BitwiseRightShift => ">>"
    case BitwiseUnsignedRightShift => ">>>"
    case BitwiseLeftShift => "<<"
    case BitwiseXor => "^"
    case Equal => "=="
    case StrictEqual => "==="
    case NotEqual => "!="
    case StrictNotEqual => "!=="
    case In => "in"
    case InstanceOf => "instanceof"
    case GreaterThan => ">"
    case LessThan => "<"
    case GreaterThanOrEqual => ">="
    case LessThanOrEqual => "<="
    case Pipeline => "|>"
  }

case class BinaryExpression(
  operator: BinaryOperator,
  left: Node with Expression | PrivateName,
  right: Node with Expression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Binary with Expression

enum LogicalOperator:
  case Or
  case And
  case NullishCoalescing

object LogicalOperator:
  def from(op: String): Option[LogicalOperator] =
    op match
      case "||" => Some(Or)
      case "&&" => Some(And)
      case "??" => Some(NullishCoalescing)
      case _ => None
  def to(op: LogicalOperator): String =
    op match {
      case Or => "||"
      case And => "&&"
      case NullishCoalescing => "??"
    }

case class LogicalExpression(
  operator: LogicalOperator,
  left: Node with Expression,
  right: Node with Expression
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Binary with Expression

case class MemberExpression(
  `object`: Node with Expression | Super,
  property: Node with Expression | Identifier | PrivateName,
  computed: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Expression with LVal

case class MetaProperty(meta: Identifier, property: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Expression

case class PrivateName(id: Identifier)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Private

case class V8IntrinsicIdentifier(name: String)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Miscellaneous

case class ModuleExpression(body: Program)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Expression
