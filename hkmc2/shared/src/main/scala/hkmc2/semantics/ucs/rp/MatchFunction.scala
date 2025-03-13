package hkmc2
package semantics
package ucs
package rp

import mlscript.utils.*, shorthands.*
import collection.mutable.{Buffer, Map as MutMap}
import Compiler.*
import Elaborator.*
import Pattern.*
import syntax.{Fun, Keyword, Literal, Tree}, Tree.{Dummy, Ident}
import Term.Missing
import utils.{TraceLogger, tl}

abstract class MatchFunction:
  /** The unique index of this match function. */
  val id: Int
  /** The function's symbol should be created before the compilation. */
  val symbol: BlockLocalSymbol
  /** The patterns recognized by this match function. The match function is
   *  supposed to return an integer from 0 to `inputPatterns.size`. */
  val inputPatterns: Vector[Pattern]
  
  def makeFunction(elaborator: Elaborator)(using Ctx): (ParamList, Term)
  
  def makeLambda(elaborator: Elaborator)(using Ctx): Term.Lam =
    val (paramList, body) = makeFunction(elaborator)
    Term.Lam(paramList, body)
  
  
object MatchFunction:
  class Forwarded(
      val id: Int,
      val symbol: BlockLocalSymbol,
      val inputPatterns: Vector[Pattern],
      val implementation: MatchFunction
  ) extends MatchFunction:
    def makeFunction(elaborator: Elaborator)(using Ctx): (ParamList, Term) =
      given State = elaborator.state
      // TODO: Implement forwarding the extracts.
      val scrutinee = VarSymbol(Ident("scrut"))
      val paramList = PlainParamList(Param(FldFlags.empty, scrutinee, N) :: Nil)
      val body =
        val dummyTuple: Tree.Tup = Tree.Tup(Dummy :: Nil)
        val dummyApp: Tree.App = Tree.App(Tree.Dummy, dummyTuple)
        val callee = implementation.symbol.ref()
        val arguments = Term.Tup(PlainFld(scrutinee.ref()) :: Nil)(dummyTuple)
        Term.App(callee, arguments)(dummyApp, FlowSymbol(s"forwarded result"))
      (paramList, body)
  
  @deprecated("This might be useless")
  class Transformed(
      val id: Int,
      val symbol: BlockLocalSymbol,
      val inputPatterns: Vector[Pattern],
      val extracts: Vector[Pattern.Path],
      val parameters: Vector[VarSymbol],
      val transformer: Term
  )

  class Implemented(
      val id: Int,
      val symbol: BlockLocalSymbol,
      val inputPatterns: Vector[Pattern]
  ) extends MatchFunction:
    private[rp] val groups = Buffer.empty[SituationGroup]
    
    def makeFunction(elaborator: Elaborator)(using Ctx): (ParamList, Term) = elaborator.tl.trace(
      pre = s"Implemented.makeFunction <<< $id, ${symbol.nme}",
      post = (_: (ParamList, Term)) => "Implemented.makeFunction >>>"
    ):
      import semantics.Pattern.*
      given TraceLogger = elaborator.tl
      given State = elaborator.state
      given Raise = elaborator.raise
      val scrutinee = VarSymbol(Ident("scrut"))
      val compiledSplit = groups.foldRight(Split.End): (group, alternative) =>
        tl.log(s"Make the split for group ${group.outermost.display}")
        group.outermost match
          case literal: Literal =>
            val pattern = Lit(literal)
            val consequent = group.makeSplit(Nil)
            Branch(scrutinee.ref(), pattern, consequent) ~: alternative
          case symbol: ClassLikeSymbol =>
            val subScrutinees = List.from:
              (0 until symbol.arity).map: i =>
                TempSymbol(N, s"t${i.toSubscriptString}")
            // TODO: Reduce the verbosity.
            val classTerm = elaborator.reference(symbol match
              case symbol: ClassSymbol => symbol
              case symbol: ModuleSymbol => symbol).getOrElse(Missing)
            val pattern = ClassLike(symbol, classTerm, S(subScrutinees), false)(Dummy)
            val consequent = group.makeSplit(subScrutinees)
            Branch(scrutinee.ref(), pattern, consequent) ~: alternative
          case Wildcard =>
            if alternative isnt Split.End then
              TODO("Report warning: The following situation groups are unreachable.")
            group.makeSplit(Nil)
      val normalize = new Normalization(elaborator)
      val normalizedSplit = tl.scoped("ucs:normalize")(normalize(compiledSplit))
      val paramList = PlainParamList(Param(FldFlags.empty, scrutinee, N) :: Nil)
      val body = Term.IfLike(Keyword.`if`, compiledSplit)(normalizedSplit)
      (paramList, body)
      
  
  class SituationGroup(
      val outermost: Situation.Outermost,
      val situations: Vector[Situation],
      val columnMatches: Vector[ColumnMatch]
  ):
    import semantics.Pattern.{Lit as LiteralPattern, Tuple as TuplePattern}
    
    val arity = outermost.arity
    
    require(situations.nonEmpty)
    require(arity == columnMatches.size)
    
    /** For the given situation, create a nested split that performs a
     *  one-to-one matching between each state and each pattern's index
     *  corresponding to the situation, applies the situation's
     *  transformation to the extracts, and finally returns the result. */
    private def makeBranch(
        stateSymbols: Vector[TempSymbol], // symbols for `s_1, s_2, ..., s_n`
        situation: Situation, // needs the `index`
        row: Int, // which number this `situation` is among all `situations`
        alternative: Split
    )(using TraceLogger): Split =
      require(stateSymbols.size == columnMatches.size, s"${stateSymbols.size} != ${columnMatches.size}")
      // All pattern mappings for the current situation.
      val patternMappings = columnMatches.iterator.map(_.patternMapping(row))
      tl.log(s"Branch: ${stateSymbols.iterator.map(_.nme).mkString("[", ", ", "]")} is ${
        patternMappings.iterator.map(_.unifiedPatternIndex).mkString("[", ", ", "]")} then ${
        situation.index}")
      stateSymbols.iterator.zip(patternMappings).zipWithIndex.foldRight(
        // This is the index of the original pattern recognized by this situation.
        Split.Else(Term.Lit(Tree.IntLit(BigInt(situation.index))))
      ):
        // TODO: The warning generated by destructing named tuples.
        case (((symbol, (targetIndex, extractMapping)), columnIndex), inner) =>
          val pattern = LiteralPattern(Tree.IntLit(BigInt(targetIndex)))
          // TODO: Apply `extractMapping`.
          Branch(symbol.ref(), pattern, inner) ~: (if columnIndex == 0 then alternative else Split.End)
    
    
    /** Build the inner pattern maching using the situations. */
    def makeSplit(subScrutinees: List[BlockLocalSymbol])(using State, TraceLogger): Split = tl.trace(
      pre = s"SituationGroup.makeSplit <<< ${subScrutinees.iterator.map(_.nme).mkString(", ")}"
    ):
      require(subScrutinees.size == columnMatches.size)
      if outermost == Wildcard && situations.size > 1 then
        TODO("Report warning: There is more than one situation in this group.")
      val symbolForTuple = TempSymbol(N, "scrut")
      // Make a tuple looks like `[f_1(x_1), f_2(x_2), f_3(x_3), f_4(x_4)]`.
      val tuple = columnMatches.iterator.zip(subScrutinees).zipWithIndex.map:
        case ((columnMatch, subScrutinee), i) =>
        val dummyTuple: Tree.Tup = Tree.Tup(Dummy :: Nil)
        val dummyApp: Tree.App = Tree.App(Tree.Dummy, dummyTuple)
        val callee = columnMatch.matchFunction.symbol.ref()
        val arguments = Term.Tup(PlainFld(subScrutinee.ref()) :: Nil)(dummyTuple)
        val call = Term.App(callee, arguments)(dummyApp, FlowSymbol(s"res#$i"))
        PlainFld(call)
      .toList |> { Term.Tup(_: List[Fld])(Tree.Tup(Nil)) }
      tl.log(s"The scrutinee: ${tuple.showDbg}")
      // Symbols for the destructed tuple elements.
      val tupleFieldSymbols = (0 until arity).map: index =>
        TempSymbol(N, s"s$index")
      .toVector
      tl.log(s"The destructed tuple elements are " + tupleFieldSymbols.iterator.map(_.nme).mkString(", "))
      // Make a split that looks like this:
      //     let scrut = [f_1(x_1), f_2(x_2)]
      //     scrut is []=2 and
      //       let s1 = scrut.1
      //       let s2 = scrut.2
      //       ... // Expand situations here
      Split.Let(
        symbolForTuple,
        tuple,
        tupleFieldSymbols.iterator.zipWithIndex.foldRight(
          situations.iterator.zipWithIndex.foldRight(Split.End):
            case ((situation, index), alternative) =>
              makeBranch(tupleFieldSymbols, situation, index, alternative)
        ):
          case ((symbol, index), alternative) =>
            val field = Ident(index.toString): Ident
            val select = Term.SynthSel(symbolForTuple.ref(), field)(N)
            Split.Let(symbol, select, alternative)
      )
