package com.rallyhealth.vapors.v1

package engine

import algebra._
import data._
import debug.DebugArgs
import dsl.{ConvertToHList, Sortable, ZipToShortest}
import lens.CollectInto
import logic.{Conjunction, Disjunction, Negation}

import cats.{Applicative, FlatMap, Foldable, Functor, SemigroupK, Traverse}
import shapeless.HList

import scala.annotation.nowarn

// TODO: Rename all DSLs and this engine from "standard" to "lossless", "mirror", or something more descriptive
//       rather than prescriptive. Many users will not find a full copy of the entire expression tree to be
//       necessary for their use cases, so this engine will probably not be as "standard" as the "simple" engine
object StandardEngine {

  @inline def apply[OP[_]]: Applied[OP] = new Applied[OP]

  final class Applied[OP[_]](@nowarn private val dummy: Boolean = true) extends AnyVal {
    def apply[PO](state: ExprState[Any, PO]): Visitor[PO, OP] = new Visitor(state)
  }

  /**
    *
    * @note In Scala 3, this might be able to just become a visitor over a context function,
    *       which would simplify all the places where I am using `implicitly`.
    */
  class Visitor[PO, OP[_]](val state: ExprState[Any, PO])
    extends CommonUncachedEngine[OP]
    with Expr.Visitor[Lambda[(`-I`, `+O`) => PO <:< I => ExprResult[PO, I, O, OP]], OP] {

    import cats.implicits._

    protected def withOutput[O](output: O): Visitor[O, OP] = new Visitor(state.swapAndReplaceOutput(output))

    protected def stateFromInput[DI, DO](
      mapInput: PO => DI,
      output: DO,
    ): ExprState[DI, DO] =
      state.withBoth(mapInput(state.output), output)

    protected def debugging[E <: Expr.AnyWith[OP]](
      expr: E,
    )(implicit
      debugArgs: DebugArgs[E, OP],
    ): DebugArgs.Invoker[E, OP, debugArgs.In, debugArgs.Out] =
      DebugArgs[OP].of(expr)(debugArgs)

    override def visitAnd[I, B, W[+_]](
      expr: Expr.And[I, B, W, OP],
    )(implicit
      logic: Conjunction[W, B, OP],
      opO: OP[W[B]],
    ): PO <:< I => ExprResult[PO, I, W[B], OP] = { implicit evPOisI =>
      val exprs = expr.leftExpr +: expr.rightExpressions
      val results = exprs.map(_.visit(this)(implicitly))
      val resultOutputs = results.map(_.state.output)
      val output = resultOutputs.reduceLeft { (acc, r) =>
        logic.and(acc, r)
      }
      val finalState = state.swapAndReplaceOutput(output)
      debugging(expr).invokeDebugger(stateFromInput((_, resultOutputs), finalState.output))
      ExprResult.And(expr, finalState, results)
    }

    override def visitAndThen[II, IO : OP, OI, OO : OP](
      expr: Expr.AndThen[II, IO, OI, OO, OP],
    )(implicit
      evIOisOI: IO <:< OI,
    ): PO <:< II => ExprResult[PO, II, OO, OP] = { implicit evPOisI =>
      val inputResult = expr.inputExpr.visit(this)(implicitly)
      val inputResultState = inputResult.state
      val inputValue = inputResultState.output
      val outputResult = expr.outputExpr.visit(withOutput(inputValue))(implicitly)
      val finalState = state.swapAndReplaceOutput(outputResult.state.output)
      debugging(expr).invokeDebugger(stateFromInput((_, inputValue), finalState.output))
      ExprResult.AndThen(expr, finalState, inputResult, outputResult)
    }

    override def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      expr: Expr.Combine[I, LI, LO, RI, RO, O, OP],
    )(implicit
      evLOisLI: LO <:< LI,
      evROisRI: RO <:< RI,
    ): PO <:< I => ExprResult[PO, I, O, OP] = { implicit evPOisI =>
      val left = expr.leftExpr.visit(this)(implicitly)
      val right = expr.rightExpr.visit(this)(implicitly)
      val leftInput: LI = left.state.output
      val rightInput: RI = right.state.output
      val output = expr.operation(leftInput, rightInput)
      // TODO: Apply justification union here
      val finalState = state.swapAndReplaceOutput(output)
      debugging(expr).invokeDebugger(stateFromInput((_, leftInput, rightInput), finalState.output))
      ExprResult.Combine(expr, finalState, left, right)
    }

    override def visitConst[O : OP](expr: Expr.Const[O, OP]): PO <:< Any => ExprResult[PO, Any, O, OP] = { _ =>
      val finalState = state.swapAndReplaceOutput(expr.value)
      debugging(expr).invokeDebugger(finalState)
      ExprResult.Const(expr, finalState)
    }

    override def visitContainsAny[I, W[+_] : Extract, C[_] : Foldable, A, B : OP](
      expr: Expr.ContainsAny[I, W, C, A, B, OP],
    ): PO <:< I => ExprResult[PO, I, B, OP] = ???

    override def visitConvert[I, O : OP](expr: Expr.Convert[I, O, OP]): PO <:< I => ExprResult[PO, I, O, OP] = {
      implicit evPOisI =>
        val o = expr.converter(state.output)
        val finalState = state.swapAndReplaceOutput(o)
        debugging(expr).invokeDebugger(finalState)
        ExprResult.Convert(expr, finalState)
    }

    override def visitCustomFunction[I, O : OP](
      expr: Expr.CustomFunction[I, O, OP],
    ): PO <:< I => ExprResult[PO, I, O, OP] = { implicit evPOisI =>
      val customFunctionOutput = expr.function(state.output)
      val finalState = state.swapAndReplaceOutput(customFunctionOutput)
      debugging(expr).invokeDebugger(finalState)
      ExprResult.CustomFunction(expr, finalState)
    }

    override def visitDefine[I, C[_] : Foldable, T](
      expr: Expr.Define[I, C, T, OP],
    )(implicit
      opF: OP[Seq[TypedFact[T]]],
    ): PO <:< I => ExprResult[PO, I, Seq[TypedFact[T]], OP] = ???

    override def visitExists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
      expr: Expr.Exists[C, A, B, OP],
    ): PO <:< C[A] => ExprResult[PO, C[A], B, OP] = { implicit evPOisI =>
      val ca: C[A] = state.output
      val (results, o) = visitExistsCommon(expr, ca) { a =>
        val conditionResult = expr.conditionExpr.visit(withOutput(a))(implicitly)
        conditionResult.state.output
      }
      val finalState = state.swapAndReplaceOutput(o)
      debugging(expr).invokeDebugger(stateFromInput((_, results), finalState.output))
      ExprResult.Exists(expr, finalState)
    }

    override def visitFilter[C[_], A, B : ExtractValue.AsBoolean, D[_]](
      expr: Expr.Filter[C, A, B, D, OP],
    )(implicit
      filter: CollectInto.Filter[C, A, D],
      opO: OP[D[A]],
    ): PO <:< C[A] => ExprResult[PO, C[A], D[A], OP] = { implicit evPOisI =>
      val o = filter.filter(state.output, { a =>
        val conditionResult = expr.conditionExpr.visit(withOutput(a))(implicitly)
        ExtractValue.asBoolean(conditionResult.state.output)
      })
      val finalState = state.swapAndReplaceOutput(o)
      debugging(expr).invokeDebugger(finalState)
      ExprResult.Filter(expr, finalState)
    }

    override def visitFlatten[C[_] : FlatMap, A](
      expr: Expr.Flatten[C, A, OP],
    )(implicit
      opCA: OP[C[A]],
    ): PO <:< C[C[A]] => ExprResult[PO, C[C[A]], C[A], OP] = { implicit evPOisCA =>
      val cca: C[C[A]] = state.output
      val ca = cca.flatMap { ca =>
        ca
      }
      val finalState = state.swapAndReplaceOutput(ca)
      debugging(expr).invokeDebugger(finalState)
      ExprResult.Flatten(expr, finalState)
    }

    override def visitFoldLeft[I, C[_] : Foldable, A, O : OP](
      expr: Expr.FoldLeft[I, C, A, O, OP],
    ): PO <:< I => ExprResult[PO, I, O, OP] = ???

    override def visitForAll[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
      expr: Expr.ForAll[C, A, B, OP],
    ): PO <:< C[A] => ExprResult[PO, C[A], B, OP] = { implicit evPOisI =>
      val ca: C[A] = state.output
      val (results, o) = visitForAllCommon(expr, ca) { a =>
        val conditionResult = expr.conditionExpr.visit(withOutput(a))(implicitly)
        conditionResult.state.output
      }
      val finalState = state.swapAndReplaceOutput(o)
      debugging(expr).invokeDebugger(stateFromInput((_, results), finalState.output))
      ExprResult.ForAll(expr, finalState)
    }

    override def visitIdentity[I : OP](expr: Expr.Identity[I, OP]): PO <:< I => ExprResult[PO, I, I, OP] = {
      implicit evPOisI =>
        val input: I = state.output
        val finalState = state.swapAndReplaceOutput(input)
        debugging(expr).invokeDebugger(finalState)
        ExprResult.Identity(expr, finalState)
    }

    override def visitIsEqual[I, V, W[+_]](
      expr: Expr.IsEqual[I, V, W, OP],
    )(implicit
      eq: EqualComparable[W, V, OP],
      opV: OP[W[V]],
      opO: OP[W[Boolean]],
    ): PO <:< I => ExprResult[PO, I, W[Boolean], OP] = { implicit evPOisI =>
      val leftResult = expr.leftExpr.visit(this)(implicitly)
      val rightResult = expr.rightExpr.visit(this)(implicitly)
      val l = leftResult.state.output
      val r = rightResult.state.output
      val o = eq.isEqual(l, r)
      val finalState = state.swapAndReplaceOutput(o)
      debugging(expr).invokeDebugger(stateFromInput((_, l, r), finalState.output))
      ExprResult.IsEqual(expr, finalState)
    }

    override def visitGetOrElse[I, O : OP](expr: Expr.GetOrElse[I, O, OP]): PO <:< I => ExprResult[PO, I, O, OP] = ???

    override def visitMapEvery[C[_] : Functor, A, B](
      expr: Expr.MapEvery[C, A, B, OP],
    )(implicit
      opO: OP[C[B]],
    ): PO <:< C[A] => ExprResult[PO, C[A], C[B], OP] = { implicit evPOisI =>
      val input: C[A] = state.output
      val results = input.map { e =>
        expr.mapExpr.visit(withOutput(e))(implicitly)
      }
      val combinedOutput = results.map(_.state.output)
      val finalState = state.swapAndReplaceOutput(combinedOutput)
      debugging(expr).invokeDebugger(stateFromInput(evPOisI, finalState.output))
      ExprResult.MapEvery(expr, finalState, results)
    }

    override def visitMatch[I, S, B : ExtractValue.AsBoolean, O](
      expr: Expr.Match[I, S, B, O, OP],
    )(implicit
      ev: S <:< I,
      opO: OP[Option[O]],
    ): PO <:< I => ExprResult[PO, I, Option[O], OP] = ???

    override def visitNot[I, B, W[+_]](
      expr: Expr.Not[I, B, W, OP],
    )(implicit
      logic: Negation[W, B, OP],
      opB: OP[W[B]],
    ): PO <:< I => ExprResult[PO, I, W[B], OP] = { implicit evPOisI =>
      val booleanResult = expr.innerExpr.visit(this)(implicitly)
      val output = booleanResult.state.output
      val negatedOutput = logic.not(output)
      val finalState = state.swapAndReplaceOutput(negatedOutput)
      debugging(expr).invokeDebugger(stateFromInput((_, output), finalState.output))
      ExprResult.Not(expr, finalState, booleanResult)
    }

    override def visitOr[I, B, W[+_]](
      expr: Expr.Or[I, B, W, OP],
    )(implicit
      logic: Disjunction[W, B, OP],
      opO: OP[W[B]],
    ): PO <:< I => ExprResult[PO, I, W[B], OP] = { implicit evPOisI =>
      val exprs = expr.leftExpr +: expr.rightExpressions
      val results = exprs.map(_.visit(this)(implicitly))
      val resultOutputs = results.map(_.state.output)
      val output = resultOutputs.reduceLeft { (acc, r) =>
        logic.or(acc, r)
      }
      val finalState = state.swapAndReplaceOutput(output)
      debugging(expr).invokeDebugger(stateFromInput((_, resultOutputs), finalState.output))
      ExprResult.Or(expr, finalState, results)
    }

    override def visitRegexMatches[I, S, O : OP](
      expr: Expr.RegexMatches[I, S, O, OP],
    ): PO <:< I => ExprResult[PO, I, O, OP] = ???

    override def visitRepeat[I, O](
      expr: Expr.Repeat[I, O, OP],
    )(implicit
      opO: OP[IterableOnce[O]],
    ): PO <:< I => ExprResult[PO, I, IterableOnce[O], OP] = ???

    override def visitSelect[I, A, B, O : OP](
      expr: Expr.Select[I, A, B, O, OP],
    ): PO <:< I => ExprResult[PO, I, O, OP] = { implicit evPOisI =>
      val inputResult = expr.inputExpr.visit(this)(implicitly)
      val inputValue = inputResult.state.output
      val selectOutput = expr.lens.get(inputValue)
      val output = expr.wrapSelected(inputValue, selectOutput)
      val finalState = state.swapAndReplaceOutput(output)
      debugging(expr).invokeDebugger(finalState.mapInput((_, inputValue, expr.lens, selectOutput)))
      ExprResult.Select(expr, finalState)
    }

    override def visitSequence[C[+_] : Applicative : SemigroupK : Traverse, I, O](
      expr: Expr.Sequence[C, I, O, OP],
    )(implicit
      opCO: OP[C[O]],
    ): PO <:< I => ExprResult[PO, I, C[O], OP] = { implicit evPOisI =>
      val co = Traverse[C].map(expr.expressions) { e =>
        e.visit(this)(implicitly)
      }
      val finalState = state.swapAndReplaceOutput(Traverse[C].map(co)(_.state.output))
      debugging(expr).invokeDebugger(finalState)
      ExprResult.Sequence(expr, finalState, co)
    }

    override def visitSizeIs[I, N : ExtractValue[*, Int], B : ExtractValue.AsBoolean : OP](
      expr: Expr.SizeIs[I, N, B, OP],
    )(implicit
      compare: SizeComparable[I, N, B],
    ): PO <:< I => ExprResult[PO, I, B, OP] = ???

    override def visitSlice[C[_] : Traverse, A, D[_]](
      expr: Expr.Slice[C, A, D, OP],
    )(implicit
      filter: CollectInto.Filter[C, A, D],
      opO: OP[D[A]],
    ): PO <:< C[A] => ExprResult[PO, C[A], D[A], OP] = ???

    override def visitSorted[C[_], A](
      expr: Expr.Sorted[C, A, OP],
    )(implicit
      sortable: Sortable[C, A],
      opAs: OP[C[A]],
    ): PO <:< C[A] => ExprResult[PO, C[A], C[A], OP] = { implicit evPOisI =>
      val sorted = sortable.sort(state.output)
      val finalState = state.swapAndReplaceOutput(sorted)
      debugging(expr).invokeDebugger(finalState)
      ExprResult.Sorted(expr, finalState)
    }

    // TODO: This requires an Arrow over the function type, which will probably require refactoring this trait
    override def visitToHList[I, L <: HList : OP](
      expr: Expr.ToHList[I, L, OP],
    )(implicit
      toHL: ConvertToHList[L],
    ): PO <:< I => ExprResult[PO, I, L, OP] = ???

    override def visitUsingDefinitions[I, O : OP](
      expr: Expr.UsingDefinitions[I, O, OP],
    ): PO <:< I => ExprResult[PO, I, O, OP] = ???

    override def visitValuesOfType[T, O](
      expr: Expr.ValuesOfType[T, O, OP],
    )(implicit
      opTs: OP[Seq[O]],
    ): PO <:< Any => ExprResult[PO, Any, Seq[O], OP] = { implicit evPOisI =>
      val matchingFacts = state.factTable.getSortedSeq(expr.factTypeSet)
      val matchingValues = matchingFacts.map(expr.transform)
      val finalState = state.swapAndReplaceOutput(matchingValues)
      debugging(expr).invokeDebugger(finalState)
      ExprResult.ValuesOfType(expr, finalState)
    }

    override def visitWhen[I, B : ExtractValue.AsBoolean, O : OP](
      expr: Expr.When[I, B, O, OP],
    ): PO <:< I => ExprResult[PO, I, O, OP] = { implicit evPOisI =>
      val initFalseResults: Seq[ExprResult[PO, I, B, OP]] = Vector()
      val initFoundResult: Option[(Option[ExprResult[PO, I, B, OP]], ExprResult[PO, I, O, OP], Int)] = None
      val (falseResults, foundResult) =
        expr.conditionBranches.toSeq.zipWithIndex.foldLeft((initFalseResults, initFoundResult)) {
          case ((results, None), (cb, idx)) =>
            val condResult = cb.whenExpr.visit(this)(implicitly)
            val condIsMet = ExtractValue.asBoolean(condResult.state.output)
            if (condIsMet) {
              (results, Some((Some(condResult), cb.thenExpr.visit(this)(implicitly), idx)))
            } else {
              (results :+ condResult, None)
            }
          case (found @ (_, Some(_)), _) => found
        }
      val (matchingCondResult, outputResult, thenExprIndex) = foundResult.getOrElse {
        (None, expr.defaultExpr.visit(this)(implicitly), expr.conditionBranches.length)
      }
      val finalState = state.swapAndReplaceOutput(outputResult.state.output)
      debugging(expr).invokeDebugger(finalState.mapInput((_, thenExprIndex)))
      ExprResult.When(expr, finalState, thenExprIndex, falseResults, matchingCondResult, outputResult)
    }

    override def visitWithinWindow[I, V, W[+_]](
      expr: Expr.WithinWindow[I, V, W, OP],
    )(implicit
      comparison: WindowComparable[W, OP],
      opV: OP[W[V]],
      opW: OP[W[Window[V]]],
      opB: OP[W[Boolean]],
    ): PO <:< I => ExprResult[PO, I, W[Boolean], OP] = { implicit evPOisI =>
      val valueResult = expr.valueExpr.visit(this)(implicitly)
      val windowResult = expr.windowExpr.visit(this)(implicitly)
      val value = valueResult.state.output
      val window = windowResult.state.output
      val output = comparison.withinWindow(value, window)
      val finalState = state.swapAndReplaceOutput(output)
      debugging(expr).invokeDebugger(stateFromInput((_, value, window), finalState.output))
      ExprResult.WithinWindow(expr, finalState, valueResult, windowResult)
    }

    override def visitZipToShortestHList[I, F[+_], WL <: HList, UL <: HList](
      expr: Expr.ZipToShortestHList[I, F, WL, UL, OP],
    )(implicit
      zip: ZipToShortest.Aux[F, WL, OP, UL],
      opO: OP[F[UL]],
    ): PO <:< I => ExprResult[PO, I, F[UL], OP] = { implicit evPOisI =>
      // TODO: Figure out how to create an Arrow for this type? Maybe a simpler visitor? Maybe I can rewrite this visitor?
      val output = zip.zipToShortestWith(expr.exprHList, this)(???)(implicitly)
      output
    }
  }
}
