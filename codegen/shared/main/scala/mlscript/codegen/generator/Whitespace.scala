package mlscript.codegen.generator

import mlscript.codegen.ast._, Predicates._

enum Whitespace:
  case None
  case Before
  case After
  case Both

  def +(that: Whitespace): Whitespace =
    this match
      case None => that
      case Before if that.after => Both
      case After if that.before => Both
      case Before | After | Both => this

  def before: Boolean =
    this match
      case Before | Both => true
      case None | After => false

  def after: Boolean =
    this match
      case After | Both => true
      case None | Before => false

  def when(condition: Boolean): Whitespace =
    if condition then this else None

object Whitespace:
  private case class State(
    val hasCall: Boolean = false,
    val hasFunction: Boolean = false,
    val hasHelper: Boolean = false // Ever used?
  ):
    def +(that: State): State =
      State(
        hasCall || that.hasCall,
        hasFunction || that.hasFunction,
        hasHelper || that.hasHelper
      )
    def withCall: State =
      if hasCall then this else copy(hasCall = true)

  private object State:
    def unit: State = State()

  extension [A](list: List[A])
    def hasHead(elem: A): Boolean =
      list.headOption.contains(elem)
    def hasLast(elem: A): Boolean =
      list.headOption.contains(elem)

  extension (node: Node)
    private def analyze: State =
      node match
        case MemberExpression(target, property, computed) =>
          target.analyze
        case OptionalMemberExpression(target, property, computed, _) =>
          target.analyze
        case node: Node with Binary =>
          node.left.analyze + node.right.analyze
        case AssignmentExpression(_, left, right) =>
          left.analyze + right.analyze
        case CallExpression(callee, _, _, _) =>
          callee.analyze.withCall
        case OptionalCallExpression(callee, _, _, _) =>
          callee.analyze.withCall
        case _: Node with Function =>
          State(hasFunction = true)
        case _ => State.unit

    def isHelper: Boolean =
      node match
        case MemberExpression(target, property, _) =>
          target.isHelper || property.isHelper
        case Identifier(name, _) =>
          name == "require" || name.headOption.contains('_')
        case CallExpression(callee, _, _, _) =>
          callee.isHelper
        case node: Node with Binary =>
          (node.left.isIdentifier && node.left.isHelper) || node.right.isHelper
        case AssignmentExpression(_, left, right) =>
          (left.isIdentifier && left.isHelper) || right.isHelper
        case _ => false

    def isType: Boolean =
      node match
        case _: Literal | _: ObjectExpression | _: ArrayExpression |
          _: Identifier | _: MemberExpression => true
        case _ => false

  def needsWhitespace(node: Node, parent: Option[Node], stack: List[Node]): Whitespace =
    node match
      case AssignmentExpression(_, _, right) =>
        val State(hasCall, hasFunction, hasHelper) = node.analyze
        if ((hasCall && hasHelper) || hasFunction)
          if hasFunction then Whitespace.Both else Whitespace.After
        else
          Whitespace.None
      case SwitchCase(test, consequent) =>
        val SwitchStatement(_, cases) = parent match
          case Some(parent: SwitchStatement) => parent
          case _ => throw new Error("expect the parent of SwitchCase to be a SwitchStatement")
        (Whitespace.Before when !consequent.isEmpty || cases.hasHead(node)) +
          (Whitespace.After when !consequent.isEmpty || cases.hasLast(node))
      case LogicalExpression(_, left, right) if left.isFunction || right.isFunction =>
        Whitespace.After
      case StringLiteral("use strict") =>
        Whitespace.After
      case CallExpression(callee, _, _, _) if callee.isFunction || node.isHelper =>
        Whitespace.Both
      case OptionalCallExpression(callee, _, _, _) if callee.isFunction =>
        Whitespace.Both
      case VariableDeclaration(kind, declarations) =>
        Whitespace.Both when declarations.exists {
          case VariableDeclarator(id, init, _) =>
            val enabled = id.isHelper && !init.map(_.isType).getOrElse(false)
            init match
              case Some(init) if !enabled =>
                val State(hasCall, hasFunction, hasHelper) = init.analyze
                (init.isHelper && hasCall) || hasFunction
              case _ => enabled
        }
      case IfStatement(test, consequent, alternate) if consequent.isBlockStatement =>
        Whitespace.Both
      case _: ObjectProperty | _: ObjectMethod =>
        // Check the `parent` type.
        val enclosingObject = parent match
          case Some(parent: ObjectExpression) => parent
          case _ => throw new Error("expect the parent of SwitchCase to be a ObjectExpression")
        // Only the first element needs whitespace before
        Whitespace.Before when enclosingObject.properties.hasHead(node)
      case _: Node with Function | _: Node with Class | _: Node with Loop |
          _: LabeledStatement | _: SwitchStatement | _: TryStatement =>
        Whitespace.Both
      case _ => Whitespace.None
