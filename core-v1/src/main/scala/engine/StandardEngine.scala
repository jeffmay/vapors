package com.rallyhealth.vapors.v1

package engine

import algebra.{EqualComparable, Expr, ExprResult, WindowComparable}
import data.{ExprState, ExtractValue, Window}
import debug.DebugArgs
import logic.{Conjunction, Disjunction, Negation}

import cats.{Foldable, Functor}
import logic.Negation
import cats.{Foldable, Functor, FunctorFilter}
import com.rallyhealth.vapors.v1.data.ExtractValue.AsBoolean

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
    extends Expr.Visitor[Lambda[(`-I`, `+O`) => PO <:< I => ExprResult[PO, I, O, OP]], OP]
    with CommonEngine[OP] {

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

    override def visitAnd[I, B, F[+_]](
      expr: Expr.And[I, B, F, OP],
    )(implicit
      logic: Conjunction[F, B, OP],
      opO: OP[F[B]],
    ): PO <:< I => ExprResult[PO, I, F[B], OP] = { implicit evPOisI =>
      val left = expr.leftExpr.visit(this)(implicitly)
      val right = expr.rightExpr.visit(this)(implicitly)
      val leftIsTrue = left.state.output
      val rightIsTrue = right.state.output
      val output = logic.and(leftIsTrue, rightIsTrue)
      val finalState = state.swapAndReplaceOutput(output)
      debugging(expr).invokeDebugger(stateFromInput((_, leftIsTrue, rightIsTrue), finalState.output))
      ExprResult.And(expr, finalState, left, right)
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

    override def visitExists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
      expr: Expr.Exists[C, A, B, OP],
    ): PO <:< C[A] => ExprResult[PO, C[A], B, OP] = { implicit evPOisI =>
      val ca: C[A] = state.output
      val (results, o, _) = visitExistsCommon(expr, ca, ()) {
        case (a, _) =>
          val conditionResult = expr.conditionExpr.visit(withOutput(a))(implicitly)
          (conditionResult.state.output, ())
      }
      val finalState = state.swapAndReplaceOutput(o)
      val debugState = stateFromInput(i => (i: C[A], results), finalState.output)
      debugging(expr).invokeDebugger(stateFromInput((_, results), finalState.output))
      ExprResult.Exists(expr, finalState)
    }

    override def visitFilter[C[_] : FunctorFilter, A, B : AsBoolean : OP](
      expr: Expr.Filter[C, A, B, OP],
    )(implicit
      opO: OP[C[A]],
    ): PO <:< C[A] => ExprResult[PO, C[A], C[A], OP] = { implicit evPOisI =>
      val ca: C[A] = state.output
      val o = ca.filter { a =>
        val conditionResult = expr.conditionExpr.visit(withOutput(a))(implicitly)
        ExtractValue.asBoolean(conditionResult.state.output)
      }
      val finalState = state.swapAndReplaceOutput(o)
      debugging(expr).invokeDebugger(finalState)
      ExprResult.Filter(expr, finalState)
    }

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

    override def visitIsEqual[I, V, F[+_]](
      expr: Expr.IsEqual[I, V, F, OP],
    )(implicit
      eq: EqualComparable[F, V],
    ): PO <:< I => ExprResult[PO, I, F[Boolean], OP] = ???

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

    override def visitNot[I, B, F[+_]](
      expr: Expr.Not[I, B, F, OP],
    )(implicit
      logic: Negation[F, B, OP],
      opB: OP[F[B]],
    ): PO <:< I => ExprResult[PO, I, F[B], OP] = { implicit evPOisI =>
      val booleanResult = expr.innerExpr.visit(this)(implicitly)
      val output = booleanResult.state.output
      val negatedOutput = logic.not(output)
      val finalState = state.swapAndReplaceOutput(negatedOutput)
      debugging(expr).invokeDebugger(stateFromInput((_, output), finalState.output))
      ExprResult.Not(expr, finalState, booleanResult)
    }

    override def visitOr[I, B, F[+_]](
      expr: Expr.Or[I, B, F, OP],
    )(implicit
      logic: Disjunction[F, B, OP],
      opO: OP[F[B]],
    ): PO <:< I => ExprResult[PO, I, F[B], OP] = { implicit evPOisI =>
      val left = expr.leftExpr.visit(this)(implicitly)
      val right = expr.rightExpr.visit(this)(implicitly)
      val leftIsTrue = left.state.output
      val rightIsTrue = right.state.output
      val output = logic.or(leftIsTrue, rightIsTrue)
      val finalState = state.swapAndReplaceOutput(output)
      debugging(expr).invokeDebugger(stateFromInput((_, leftIsTrue, rightIsTrue), finalState.output))
      ExprResult.Or(expr, finalState, left, right)
    }

    override def visitSelect[I, O : OP](expr: Expr.Select[I, O, OP]): PO <:< I => ExprResult[PO, I, O, OP] = {
      implicit evPOisI =>
        val output = expr.lens.get(state.output)
        val finalState = state.swapAndReplaceOutput(output)
        debugging(expr).invokeDebugger(stateFromInput((_, expr.lens), finalState.output))
        ExprResult.Select(expr, finalState)
    }

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

    override def visitWithinWindow[I, V, F[+_]](
      expr: Expr.WithinWindow[I, V, F, OP],
    )(implicit
      comparison: WindowComparable[F, OP],
      opV: OP[F[V]],
      opW: OP[F[Window[V]]],
      opB: OP[F[Boolean]],
    ): PO <:< I => ExprResult[PO, I, F[Boolean], OP] = { implicit evPOisI =>
      val valueResult = expr.valueExpr.visit(this)(implicitly)
      val windowResult = expr.windowExpr.visit(this)(implicitly)
      val value = valueResult.state.output
      val window = windowResult.state.output
      val output = comparison.withinWindow(value, window)
      val finalState = state.swapAndReplaceOutput(output)
      debugging(expr).invokeDebugger(stateFromInput((_, value, window), finalState.output))
      ExprResult.WithinWindow(expr, finalState, valueResult, windowResult)
    }
  }
}
