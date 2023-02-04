package mlscript.codegen.generator

import mlscript.codegen.ast._

object Parentheses:
  private val precedence = Map[BinaryOperator | LogicalOperator, Int](
    (LogicalOperator.Or -> 0),
    LogicalOperator.NullishCoalescing -> 0,
    BinaryOperator.Pipeline -> 0,
    LogicalOperator.And -> 1,
    BinaryOperator.BitwiseOr -> 2,
    BinaryOperator.BitwiseXor -> 3,
    BinaryOperator.BitwiseAnd -> 4,
    BinaryOperator.Equal -> 5,
    BinaryOperator.StrictEqual -> 5,
    BinaryOperator.NotEqual -> 5,
    BinaryOperator.StrictNotEqual -> 5,
    BinaryOperator.LessThan -> 6,
    BinaryOperator.GreaterThan -> 6,
    BinaryOperator.LessThanOrEqual -> 6,
    BinaryOperator.GreaterThanOrEqual -> 6,
    BinaryOperator.In -> 6,
    BinaryOperator.InstanceOf -> 6,
    BinaryOperator.BitwiseRightShift -> 7,
    BinaryOperator.BitwiseLeftShift -> 7,
    BinaryOperator.BitwiseUnsignedRightShift -> 7,
    BinaryOperator.Plus -> 8,
    BinaryOperator.Minus -> 8,
    BinaryOperator.Multiplication -> 9,
    BinaryOperator.Divide -> 9,
    BinaryOperator.Modolus -> 9,
    BinaryOperator.Exponentiation -> 10,
  )

  private def getPrecedence(op: BinaryOperator | LogicalOperator): Int =
    precedence.get(op).getOrElse(throw Error(s"no precedence for $op"))

  private def isOrHasCallExpression(node: Node): Boolean =
    node match
      case ce: CallExpression => true
      case MemberExpression(obj, _, _) => isOrHasCallExpression(obj)
      case _ => false

  def needsParens(node: Node, parent: Option[Node], stack: List[Node]): Boolean =
    parent match
      case None => false
      case Some(NewExpression(callee, _, _, _))
        if callee == node && isOrHasCallExpression(node) => true
      case Some(parent) => handleParens(node, parent, stack)

  private def hasPostfixPart(node: Node, parent: Node) =
    parent match
      case MemberExpression(obj, _, _) if (obj == node) => true
      case OptionalMemberExpression(obj, _, _, _) if (obj == node) => true
      case CallExpression(callee, _, _, _) if (callee == node) => true
      case OptionalCallExpression(callee, _, _, _) if (callee == node) => true
      case NewExpression(callee, _, _, _) if (callee == node) => true
      case TaggedTemplateExpression(tag, _, _) if (tag == node) => true
      case _: TSNonNullExpression => true
      case _ => false

  private object CheckBit:
    val expressionStatement: Int = 1 << 0
    val arrowBody: Int = 1 << 1
    val exportDefault: Int = 1 << 2
    val forHead: Int = 1 << 3
    val forInHead: Int = 1 << 4
    val forOfHead: Int = 1 << 5

  private def isFirstInContext(stack: List[Node], checkBits: Int): Option[Boolean] =
    def shouldRec(node: Node, parent: Node): Boolean =
      parent match
        case MemberExpression(target, _, _) => target == node
        case OptionalMemberExpression(target, _, _, _) => target == node
        case CallExpression(callee, _, _, _) => callee == node
        case OptionalCallExpression(callee, _, _, _) => callee == node
        case TaggedTemplateExpression(tag, _, _) => tag == node
        case _: TSNonNullExpression => true
        case SequenceExpression(first :: _) => first == node
        case UpdateExpression(_, _, prefix) => !prefix
        case t: Node with Conditional => t.test == node
        case t: Node with Binary => t.left == node
        case AssignmentExpression(_, left, _) => left == node
        case _ => false
    def rec(stack: List[Node]): Option[Boolean] =
      stack match
        case node :: (tail @ (parent :: rest)) =>
          parent match
            case ExpressionStatement(expression)
              if (checkBits & CheckBit.expressionStatement) != 0 && expression == node => Some(true)
            case ExportDefaultDeclaration(declaration)
              if (checkBits & CheckBit.exportDefault) != 0 && declaration == node => Some(true)
            case ArrowFunctionExpression(_, body, _, _)
              if (checkBits & CheckBit.arrowBody) != 0 && body == node => Some(true)
            case ForStatement(init, _, _, _)
              if (checkBits & CheckBit.forHead) != 0 && init == node => Some(true)
            case ForInStatement(left, _, _)
              if (checkBits & CheckBit.forInHead) != 0 && left == node => Some(true)
            case ForOfStatement(left, _, _, _)
              if (checkBits & CheckBit.forOfHead) != 0 && left == node => Some(true)
            case _ if !rest.isEmpty && shouldRec(node, parent) => rec(tail)
            case _ => Some(false)
        case _ :: Nil => None
        case Nil => None
    rec(stack)

  private def handleParens(node: Node, parent: Node, stack: List[Node]): Boolean =
    node match
      case _: UpdateExpression =>
        hasPostfixPart(node, parent) || (parent match
          case node: Node with Class => node.superClass.contains(node)
          case _ => false
        )
      case _: ObjectExpression =>
        val bits = CheckBit.expressionStatement | CheckBit.arrowBody
        isFirstInContext(stack, bits).getOrElse(false)
      case DoExpression(_, false) =>
        isFirstInContext(stack, CheckBit.expressionStatement).getOrElse(false)
      case BinaryExpression(BinaryOperator.In, _, _)
        if (parent match {
          case _: VariableDeclarator | _: Node with For => true
          case _ => false
        }) => true
      case node: Node with Binary =>
        // This is ugly but should work.
        (node match
          case LogicalExpression(operator, _, _) =>
            parent match
              case _: TSAsExpression | _: TSSatisfiesExpression | _: TSTypeAssertion => true
              case _ =>
                operator match
                  case LogicalOperator.Or => parent match
                    case LogicalExpression(LogicalOperator.NullishCoalescing, _, _) => true
                    case LogicalExpression(LogicalOperator.And, _, _) => true
                    case _ => false
                  case LogicalOperator.And => parent match
                    case LogicalExpression(LogicalOperator.NullishCoalescing, _, _) => true
                    case _ => false
                  case LogicalOperator.NullishCoalescing => parent match
                    case LogicalExpression(operator, _, _) =>
                      operator != LogicalOperator.NullishCoalescing
                    case _ => false
          case _ => false) ||
        (parent match
          case BinaryExpression(BinaryOperator.Exponentiation, left, _)
            if node.operator == BinaryOperator.Exponentiation => left == node
          case parent: Node with Class if parent.superClass.contains(node) => true
          case _: Node with UnaryLike | _: AwaitExpression if hasPostfixPart(node, parent) => true
          case parent: Node with Binary =>
            val parentPrec = getPrecedence(parent.operator)
            val nodePrec = getPrecedence(node.operator)
            val parentIsNotLogicalExpr = parent match
              case _: LogicalExpression => false
              case _ => true
            (parentPrec == nodePrec && parent.right == node && parentIsNotLogicalExpr) || parentPrec > nodePrec
          case _ => false)
      case _: TSAsExpression | _: TSSatisfiesExpression | _: TSTypeAssertion => true
      case _: TSUnionType | _: TSIntersectionType => parent match
        case _: TSArrayType | _: TSOptionalType | _: TSIntersectionType | _: TSUnionType | _: TSRestType => true
        case _ => false
      case _: TSInferType => parent match
        case _: TSArrayType | _: TSOptionalType => true
        case _ => false
      case _: TSInstantiationExpression => parent match
        case parent: CallExpression if parent.typeParameters.isDefined => true
        case parent: OptionalCallExpression if parent.typeParameters.isDefined => true
        case parent: NewExpression if parent.typeParameters.isDefined => true
        case parent: TSInstantiationExpression if parent.typeParameters.isDefined => true
        case _ => false
      case _: SequenceExpression => parent match
        // Although parentheses wouldn"t hurt around sequence
        // expressions in the head of for loops, traditional style
        // dictates that e.g. i++, j++ should not be wrapped with
        // parentheses.
        case _: ForStatement => false
        case _: ThrowStatement => false
        case _: ReturnStatement => false
        case IfStatement(test, _, _) if (test == node) => false
        case WhileStatement(test, _) if (test == node) => false
        case ForInStatement(_, right, _) if (right == node) => false
        case SwitchStatement(dis, _) if (dis == node) => false
        case ExpressionStatement(exp) if (exp == node) => false
        // Otherwise err on the side of overparenthesization, adding
        // explicit exceptions above if this proves overzealous.
        case _ => true
      case _: YieldExpression =>
        parent match
          case _: Node with Binary => true
          case _: Node with UnaryLike => true
          case AwaitExpression(expression) => expression == node
          case ConditionalExpression(test, _, _) => test == node
          case t: Class => t.superClass.contains(node)
          case _ => hasPostfixPart(node, parent)
      case _: AwaitExpression => // Same as the previous case.
        parent match
          case _: Node with Binary => true
          case _: Node with UnaryLike => true
          case AwaitExpression(expression) => expression == node
          case ConditionalExpression(test, _, _) => test == node
          case t: Class => t.superClass.contains(node)
          case _ => hasPostfixPart(node, parent)
      case _: ClassExpression =>
        val bits = CheckBit.expressionStatement | CheckBit.exportDefault
        isFirstInContext(stack, bits).getOrElse(false)
      case node: Node with UnaryLike =>
        parent match
          case BinaryExpression(_, left, _) => left == node
          case parent: Node with Class => parent.superClass.contains(node)
          case _ => hasPostfixPart(node, parent)
      case _: FunctionExpression =>
        val bits = CheckBit.expressionStatement | CheckBit.exportDefault
        isFirstInContext(stack, bits).getOrElse(false)
      case _: ArrowFunctionExpression => parent match
        case _: Node with ExportDeclaration => true
        // The same as ConditionalExpression.
        case _: Node with UnaryLike | _: Node with Binary | _: AwaitExpression |
          _: TSAsExpression | _: TSSatisfiesExpression | _: TSTypeAssertion => true
        case ConditionalExpression(test, _, _) => test == node
        case parent: Node with Class => parent.superClass.contains(node)
        case _ => hasPostfixPart(node, parent)
      case _: ConditionalExpression => parent match
        case _: Node with UnaryLike | _: Node with Binary | _: AwaitExpression |
          _: TSAsExpression | _: TSSatisfiesExpression | _: TSTypeAssertion => true
        case ConditionalExpression(test, _, _) => test == node
        case parent: Node with Class => parent.superClass.contains(node)
        case _ => hasPostfixPart(node, parent)
      case _: OptionalMemberExpression =>
        parent match
          case CallExpression(callee, _, _, _) if (callee == node) => true
          case MemberExpression(obj, _, _) if (obj == node) => true
          case _ => false
      case AssignmentExpression(_, left, _) =>
        left match
          case _: ObjectPattern => true
          case _ => parent match // The same as ConditionalExpression
            case _: Node with UnaryLike | _: Node with Binary | _: AwaitExpression |
              _: TSAsExpression | _: TSSatisfiesExpression | _: TSTypeAssertion => true
            case ConditionalExpression(test, _, _) => test == node
            case parent: Node with Class => parent.superClass.contains(node)
            case _ => hasPostfixPart(node, parent)
      case Identifier(name, _) =>
        // Three corner cases:
        // 1. `(fn) = function () {}`
        // 2. Non-strict code allows the identifier `let`
        // 3. `for (async of => {};;)`
        // If we don't generate code like this, it won't happen.
        false
      case _ => false
