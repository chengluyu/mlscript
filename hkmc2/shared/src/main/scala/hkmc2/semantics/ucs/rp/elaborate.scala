package hkmc2
package semantics
package ucs
package rp

import mlscript.utils.*, shorthands.*, hkmc2.utils.*
import syntax.Tree, Tree.*
import HelperExtractors.*, Message.MessageContext
import collection.mutable.Map as MutMap
import Elaborator.{Ctx, Ctxl, State}

def elaborate(
    patternParams: List[Param],
    extractParams: List[Param],
    definition: Tree,
    elaborator: Elaborator
)(using ctx: Ctx, state: State, raise: Raise): Pattern =
  import Pattern.*, elaborator.tl.*
  
  type Ctor = Ident | Sel
  type PathMap = Map[VarSymbol, Path]
  
  extension (left: PathMap)
    infix def &(right: PathMap): PathMap =
      val intersection = left.keySet intersect right.keySet
      if intersection.nonEmpty then
        error((intersection.iterator.map: symbol =>
          msg"The symbol is bound more than once." -> symbol.toLoc).toSeq*)
      (left.toSeq ++ right).toMap
  
  def subPatterns(params: IterableOnce[Tree])(using extractParams: Opt[List[Param]]): (Vector[Pattern], PathMap) =
    params.iterator.zipWithIndex.foldLeft[(Vector[Pattern], PathMap)]((Vector.empty, Map.empty)):
      case ((patterns, accMap), (tree, index)) => pattern(tree) match
        case (pattern, map) => (patterns :+ pattern) -> (accMap & map.view.mapValues(Path.Select(index, _)).toMap)
    
  def pattern(tree: Tree)(using extractParams: Opt[List[Param]]): (Pattern, PathMap) = trace(
    pre = s"pattern <<< ${tree.showDbg}",
    post = (res: (Pattern, PathMap)) => s"pattern >>>"
  ):
    tree.deparenthesized match
      case Under() => Wildcard -> Map.empty
      case left or right =>
        val (leftPattern, firstMap) = pattern(left)
        val (rightPattern, secondMap) = pattern(right)
        // Merge two maps and report symbols that are not bound in the right alternative.
        val mergedMap = firstMap.iterator.foldLeft[PathMap](Map.empty):
          case (accMap, symbol -> firstPath) => secondMap.get(symbol) match
            case S(secondPath) => accMap + (symbol -> Path.Diverge(firstPath, secondPath))
            case N =>
              error(msg"This symbol is not bound in the second alternative" -> symbol.toLoc)
              accMap
        // Check symbols that are not bound in the left alternative.
        secondMap.iterator.foreach:
          case symbol -> secondPath =>
            if firstMap.contains(symbol) then
              error(msg"This symbol is not bound in the first alternative" -> symbol.toLoc)
        Alternative(leftPattern, rightPattern) -> mergedMap
      case tree as (id @ Ident(name)) =>
        val (subjectPattern, subjectMap) = pattern(tree)
        extractParams match
          case S(params) => params.find(_.sym.name == name) match
            case S(Param(_, symbol, _)) =>
              subjectPattern -> (subjectMap + (symbol -> Path.End))
            case N =>
              error(msg"Unknown extraction name: $name" -> id.toLoc)
              subjectPattern -> subjectMap
          case N => subjectPattern -> (subjectMap + (VarSymbol(id) -> Path.End))
      case Tup(input :: Nil) `=>` output =>
        extractParams match
          case S(params) => error(
            msg"Pattern transformation appears here" -> output.toLoc,
            msg"but it conflicts with the extraction parameter list" -> Loc(params))
          case N => ()
        val (inputPattern, inputMap) = pattern(input)(using N)
        log("inputMap = " + inputMap)
        val outputCtx = ctx ++ inputMap.keys.map(symbol => symbol.name -> symbol)
        val term = scoped("npc:term"):
          elaborator.term(output, false, false)(using outputCtx)
        Transformed(inputPattern, inputMap.values.toVector)(inputMap.keys.toVector, S(term)) -> Map.empty
      case literal: syntax.Literal => Pattern.Constant(literal) -> Map.empty
      case ctor: Ctor => app(ctor, Nil)
      case App(ctor: Ctor, Tup(params)) => app(ctor, params)
      case other =>
        log(other.showDbg)
        error(msg"Unrecognized pattern (${other.describe})" -> other.toLoc)
        Wildcard -> Map.empty
  
  /** Get the symbol of constructor-like terms. */
  def resolve(ctor: Ctor)(using extractionParamsOpt: Opt[List[Param]]): Opt[MatchableSymbol] = (ctor match
    case id: Ident => ctx.get(id.name).flatMap(_.symbol) orElse state.builtinOpsMap.get(id.name)
    case other => scoped("ucs:npc:cls"):
      elaborator.cls(ctor, inAppPrefix = false).symbol).flatMap(_.asClsLike)
  
  /** Look up the symbol from the context. */
  def lookup(ctor: Ctor, params: Ls[Tree])(using extractionParamsOpt: Opt[List[Param]]): Ctxl[Opt[(Pattern, PathMap)]] =
    resolve(ctor) match
      case S(symbol: ClassLikeSymbol) => 
        if params.size != symbol.arity then
          error(msg"The class `${symbol.nme}` expected ${symbol.arity.toString} arguments." -> symbol.toLoc,
            msg"But only ${params.size.toString} sub-pattern${
              if params.size == 1 then " is" else "s are"
            } given." -> Loc(params))
        val (patterns, map) = subPatterns(params)
        S(Constructor(symbol, patterns) -> map)
      case S(symbol: PatternSymbol) =>
        if params.size != symbol.parameterCount then
          error(msg"The numbers of parameters mismatch." -> Loc(params))
        val (patternTrees, extractionTrees) = params.iterator.splitAt(symbol.patternParameterCount)
        val patternArguments = patternTrees.map: tree =>
          val (patternArg, map) = pattern(tree)(using N)
          if map.nonEmpty then
            // TODO: Use more precise locations from `map`.
            error(msg"Free variables found in pattern parameters." -> tree.toLoc)
          patternArg
        .toList
        val (extractionArguments, map) = subPatterns(extractionTrees)
        S(NonTerminal(symbol, patternArguments, extractionArguments) -> map)
      case N => extractionParamsOpt match
        case S(_) =>
          error(msg"Name not found: ${ctor.showDbg}" -> ctor.toLoc)
          N
        case N => ctor match
          case id: Ident =>
            S(Wildcard -> Map(VarSymbol(id) -> Path.End))
          case _: Sel =>
            error(msg"Name not found: ${ctor.showDbg}" -> ctor.toLoc)
            N
  
  /** Process constructor-like patterns. */
  def app(ctor: Ctor, params: Ls[Tree])(using extractionParamsOpt: Opt[List[Param]]): (Pattern, PathMap) = ctor match
    // Search in the pattern parameters first.
    case id @ Ident(name) => patternParams.find(_.sym.name == name) match
      case S(Param(_, symbol, _)) =>
        // TODO: Handle/check extractions of pattern parameters.
        val (patterns, map) = subPatterns(params)
        NonTerminal(symbol, Nil, patterns) -> map
      // Check if the extraction parameters are provided.
      case N => extractionParamsOpt match
        // Search in the provided extraction parameters next.
        case S(extractionParams) => extractionParams.find(_.sym.name == name) match
          case S(Param(_, symbol, _)) =>
            if params.isEmpty then
              Wildcard -> Map(symbol -> Path.End)
            else
              error(msg"Extraction variables cannot have parameters." -> ctor.toLoc)
              Wildcard -> Map.empty
          case N => lookup(ctor, params).getOrElse:
            error(msg"Unknown extraction name: ${ctor.toString}" -> ctor.toLoc)
            Wildcard -> Map.empty
        // Search in the elaboration context.
        case N =>
          lookup(ctor, params).getOrElse:
            error(msg"Constructor not found: ${ctor.toString}" -> ctor.toLoc)
            Wildcard -> Map.empty
    case _: Sel => lookup(ctor, params).getOrElse:
      error(msg"Name not found: ${ctor.toString}" -> ctor.toLoc)
      Wildcard -> Map.empty
  
  scoped("ucs:npc"):
    val (topLevelPattern, paramPathMap) = pattern(definition)(using if extractParams.isEmpty then N else S(extractParams))
    val (parameters, paths) = paramPathMap.unzip
    if extractParams.isEmpty then
      require(paramPathMap.isEmpty)
      topLevelPattern
    else
      Transformed(topLevelPattern, paths.toVector)(parameters.toVector, N)
