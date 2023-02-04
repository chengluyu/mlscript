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
  id: Identifier,
  superClass: Option[Node with Expression] = None,
  body: ClassBody,
  decorators: List[Decorator] = List(),
  implements: Option[List[TSExpressionWithTypeArguments]] = None,
  superTypeParameters: Option[TSTypeParameterInstantiation] = None,
  typeParameters: Option[TSTypeParameterDeclaration] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Class with Expression

case class ClassDeclaration(
  id: Identifier,
  superClass: Option[Node with Expression] = None,
  body: ClassBody,
  decorators: List[Decorator] = List(),
  `abstract`: Boolean = false,
  declare: Boolean = false,
  implements: Option[List[TSExpressionWithTypeArguments]] = None,
  superTypeParameters: Option[TSTypeParameterInstantiation] = None,
  typeParameters: Option[TSTypeParameterDeclaration] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Scopable with Class with Statement with Declaration

case class ClassBody(
  body: List[ClassMethod | ClassPrivateMethod | ClassProperty | ClassPrivateProperty | ClassAccessorProperty | TSDeclareMethod | TSIndexSignature | StaticBlock]
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized

case class ClassProperty(
  key: Identifier | StringLiteral | NumericLiteral | BigIntLiteral | Node with Expression,
  value: Option[Node with Expression] = None,
  decorators: List[Decorator] = List(),
  typeAnnotation: Option[TSTypeAnnotation] = None,
  computed: Boolean = false,
  static: Boolean = false,
  `abstract`: Boolean = false,
  accessibility: Option[AccessModifier] = None,
  declare: Boolean = false,
  definite: Boolean = false,
  optional: Boolean = false,
  `override`: Boolean = false,
  readonly: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Property

case class ClassAccessorProperty(
  key: Identifier | StringLiteral | NumericLiteral | BigIntLiteral | Node with Expression | PrivateName,
  value: Option[Node with Expression] = None,
  decorators: List[Decorator] = List(),
  typeAnnotation: Option[TSTypeAnnotation] = None,
  computed: Boolean = false,
  static: Boolean = false,
  `abstract`: Boolean = false,
  accessibility: Option[AccessModifier] = None,
  declare: Boolean = false,
  definite: Boolean = false,
  optional: Boolean = false,
  `override`: Boolean = false,
  readonly: Boolean = false
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Property with Accessor

case class ClassPrivateMethod(
  kind: ClassPrivateMethodKind = ClassPrivateMethodKind.Method,
  key: PrivateName,
  params: List[Identifier | Node with Pattern | RestElement | TSParameterProperty],
  val body: BlockStatement,
  static: Boolean = false,
  `abstract`: Boolean = false,
  access: Option[AccessModifier] = None,
  async: Boolean = false,
  computed: Boolean = false,
  decorators: List[Decorator] = List(),
  generator: Boolean = false,
  optional: Boolean = false,
  `override`: Boolean = false,
  returnType: Option[TSTypeAnnotation] = None,
  typeParameters: Option[TSTypeParameterDeclaration] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Function with Scopable with BlockParent with FunctionParent with Method with Private

case class ClassPrivateProperty(
  key: PrivateName,
  value: Option[Node with Expression] = None,
  decorators: List[Decorator] = List(),
  typeAnnotation: Option[TSTypeAnnotation] = None,
  static: Boolean = false,
  definite: Option[Boolean] = None,
  readonly: Option[Boolean] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Property with Private

case class ClassMethod(
  kind: ClassMethodKind = ClassMethodKind.Method,
  key: Identifier | StringLiteral | NumericLiteral | BigIntLiteral | Node with Expression,
  params: List[Identifier | Node with Pattern | RestElement | TSParameterProperty],
  val body: BlockStatement,
  computed: Boolean = false,
  static: Boolean = false,
  generator: Boolean = false,
  async: Boolean = false,
  `abstract`: Boolean = false,
  access: Option[AccessModifier] = None,
  decorators: List[Decorator] = List(),
  optional: Boolean = false,
  `override`: Boolean = false,
  returnType: Option[TSTypeAnnotation] = None,
  typeParameters: Option[TSTypeParameterDeclaration] = None
)(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation]) extends Node with Standardized with Function with Scopable with BlockParent with FunctionParent with Method

case class StaticBlock(body: List[Node with Statement])(val start: Option[Int], val end: Option[Int], val location: Option[SourceLocation])
  extends Node with Standardized with Scopable with BlockParent with FunctionParent
