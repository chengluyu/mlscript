package mlscript.codegen.ast

import mlscript.codegen.ast._
import mlscript.codegen.{Position => SourcePosition, Location => SourceLocation}

enum ClassPrivateMethodKind:
  case Getter
  case Setter
  case Method

enum AccessModifier:
  case Public
  case Private
  case Protected

enum ClassMethodKind:
  case Method
  case Getter
  case Setter
  case Constructor

case class ClassExpression(
  val id: Option[Identifier] = None,
  val superClass: Option[Node with Expression] = None,
  val body: ClassBody,
  val decorators: Option[List[Decorator]] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Class with Expression:
  var implements: Option[List[TSExpressionWithTypeArguments]] = None
  var superTypeParameters: Option[TSTypeParameterInstantiation] = None
  var typeParameters: Option[TSTypeParameterDeclaration | Noop] = None

case class ClassDeclaration(
  val id: Identifier,
  val superClass: Option[Node with Expression] = None,
  val body: ClassBody,
  val decorators: Option[List[Decorator]] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Class with Statement with Declaration:
  var `abstract`: Option[Boolean] = None
  var declare: Option[Boolean] = None
  var implements: Option[List[TSExpressionWithTypeArguments]] = None
  var superTypeParameters: Option[TSTypeParameterInstantiation] = None
  var typeParameters: Option[TSTypeParameterDeclaration | Noop] = None

case class ClassBody(
  val body: List[ClassMethod | ClassPrivateMethod | ClassProperty | ClassPrivateProperty | ClassAccessorProperty | TSDeclareMethod | TSIndexSignature | StaticBlock]
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized

case class ClassProperty(
  val key: Identifier | StringLiteral | NumericLiteral | BigIntLiteral | Node with Expression,
  val value: Option[Node with Expression] = None,
  val decorators: Option[List[Decorator]] = None,
  val computed: Boolean = false,
  val static: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Property:
  var `abstract`: Option[Boolean] = None
  var accessibility: Option[AccessModifier] = None
  var declare: Option[Boolean] = None
  var definite: Option[Boolean] = None
  var optional: Option[Boolean] = None
  var `override`: Option[Boolean] = None
  var readonly: Option[Boolean] = None

case class ClassAccessorProperty(
  val key: Identifier | StringLiteral | NumericLiteral | BigIntLiteral | Node with Expression | PrivateName,
  val value: Option[Node with Expression] = None,
  val decorators: Option[List[Decorator]] = None,
  val computed: Boolean = false,
  val static: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Property with Accessor:
  var `abstract`: Option[Boolean] = None
  var accessibility: Option[AccessModifier] = None
  var declare: Option[Boolean] = None
  var definite: Option[Boolean] = None
  var optional: Option[Boolean] = None
  var `override`: Option[Boolean] = None
  var readonly: Option[Boolean] = None

case class ClassPrivateMethod(
  val kind: Option[ClassPrivateMethodKind] = Some(ClassPrivateMethodKind.Method),
  val key: PrivateName,
  val params: List[Identifier | Node with Pattern | RestElement | TSParameterProperty],
  val body: BlockStatement,
  val static: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Function with Scopable with BlockParent with FunctionParent with Method with Private:
  var `abstract`: Option[Boolean] = None
  var access: Option[AccessModifier] = None
  var accessibility: Option[AccessModifier] = None
  var async: Option[Boolean] = None
  var computed: Option[Boolean] = None
  var decorators: Option[List[Decorator]] = None
  var generator: Option[Boolean] = None
  var optional: Option[Boolean] = None
  var `override`: Option[Boolean] = None
  var returnType: Option[TSTypeAnnotation | Noop] = None
  var typeParameters: Option[TSTypeParameterDeclaration | Noop] = None


case class ClassPrivateProperty(
  val key: PrivateName,
  val value: Option[Node with Expression] = None,
  val decorators: Option[List[Decorator]] = None,
  val static: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Property with Private:
  var definite: Option[Boolean] = None
  var readonly: Option[Boolean] = None

case class ClassMethod(
  val kind: Option[ClassMethodKind] = Some(ClassMethodKind.Method),
  val key: Identifier | StringLiteral | NumericLiteral | BigIntLiteral | Node with Expression,
  val params: List[Identifier | Node with Pattern | RestElement | TSParameterProperty],
  val body: BlockStatement,
  val computed: Boolean = false,
  val static: Boolean = false,
  val generator: Boolean = false,
  val async: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Function with Scopable with BlockParent with FunctionParent with Method:
  var `abstract`: Option[Boolean] = None
  var access: Option[AccessModifier] = None
  var accessibility: Option[AccessModifier] = None
  var decorators: Option[List[Decorator]] = None
  var optional: Option[Boolean] = None
  var `override`: Option[Boolean] = None
  var returnType: Option[TSTypeAnnotation | Noop] = None
  var typeParameters: Option[TSTypeParameterDeclaration | Noop] = None

case class StaticBlock(val body: List[Node with Statement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
    extends Node with Standardized with Scopable with BlockParent with FunctionParent
