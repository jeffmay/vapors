package com.rallyhealth.vapors.v1

package engine

import algebra.{Expr, ExprResult}
import data.{ExprState, Window}

import cats.{Foldable, Functor}
import com.rallyhealth.vapors.v1.logic.Negation

import scala.annotation.nowarn

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
    extends Expr.Visitor[Lambda[(`-I`, `+O`) => PO <:< I => ExprResult[PO, I, O, OP]], OP] {

    import cats.implicits._

    protected def withState[O](state: ExprState[Any, O]): Visitor[O, OP] = new Visitor(state)

    override def visitAnd[I](
      expr: Expr.And[I, OP],
    )(implicit
      opO: OP[Boolean],
    ): PO <:< I => ExprResult[PO, I, Boolean, OP] = { implicit evPOisI =>
      val left = expr.leftExpr.visit(this)(implicitly)
      val right = expr.rightExpr.visit(this)(implicitly)
      val output = left.state.output || right.state.output
      // TODO: Do justification here
      val newState = state.swapAndReplaceOutput(output)
      expr.debugging.attach(newState)
      ExprResult.And(expr, newState, left, right)
    }

    override def visitAndThen[II, IO : OP, OI, OO : OP](
      expr: Expr.AndThen[II, IO, OI, OO, OP],
    )(implicit
      evAOisBI: IO <:< OI,
    ): PO <:< II => ExprResult[PO, II, OO, OP] = { implicit evPOisI =>
      val inputResult = expr.inputExpr.visit(this)(implicitly)
      val outputResult = expr.outputExpr.visit(withState(inputResult.state))(implicitly)
      val newState = state.swapAndReplaceOutput(outputResult.state.output)
      expr.debugging.attach(
        newState.withBoth(
          (inputResult.state.input, outputResult.state.input),
          inputResult.state.output,
        ),
      )
      ExprResult.AndThen(expr, newState, inputResult, outputResult)
    }

    override def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      expr: Expr.Combine[I, LI, LO, RI, RO, O, OP],
    )(implicit
      evLOisLI: LO <:< LI,
      evROisRI: RO <:< RI,
    ): PO <:< I => ExprResult[PO, I, O, OP] = { implicit evPOisI =>
      val left = expr.leftExpr.visit(this)(implicitly)
      val right = expr.rightExpr.visit(this)(implicitly)
      val output = expr.operation(left.state.output, right.state.output)
      // TODO: Apply justification union here
      val newState = state.swapAndReplaceOutput(output)
      expr.debugging.attach(newState.withInput((state.output, left.state.output, right.state.output)))
      ExprResult.Combine(expr, newState, left, right)
    }

    override def visitConst[O : OP](expr: Expr.Const[O, OP]): PO <:< Any => ExprResult[PO, Any, O, OP] = { _ =>
      val newState = state.swapAndReplaceOutput(expr.value)
      expr.debugging.attach(newState)
      ExprResult.Const(expr, newState)
    }

    override def visitCustomFunction[I, O : OP](
      expr: Expr.CustomFunction[I, O, OP],
    ): PO <:< I => ExprResult[PO, I, O, OP] = { implicit evPOisI =>
      val customFunctionOutput = expr.function(state.output)
      val newState = state.swapAndReplaceOutput(customFunctionOutput)
      expr.debugging.attach(newState)
      ExprResult.CustomFunction(expr, newState)
    }

    override def visitExists[C[_] : Foldable, A](
      expr: Expr.Exists[C, A, OP],
    )(implicit
      opO: OP[Boolean],
    ): PO <:< C[A] => ExprResult[PO, C[A], Boolean, OP] = { implicit evPOisI =>
      // TODO: Apply justification logic here
      val output = evPOisI(state.output).exists { e =>
        val conditionState = withState(state.withOutput(e))
        expr.conditionExpr.visit(conditionState)(implicitly).state.output
      }
      val newState = state.swapAndReplaceOutput(output)
      expr.debugging.attach(newState)
      ExprResult.Exists(expr, newState)
    }

    override def visitForAll[C[_] : Foldable, A](
      expr: Expr.ForAll[C, A, OP],
    )(implicit
      opO: OP[Boolean],
    ): PO <:< C[A] => ExprResult[PO, C[A], Boolean, OP] = { implicit evPOisI =>
      // TODO: Apply justification logic here
      val output = evPOisI(state.output).forall { e =>
        val conditionVisitor = withState(state.withOutput(e))
        expr.conditionExpr.visit(conditionVisitor)(implicitly).state.output
      }
      ExprResult.ForAll(expr, state.swapAndReplaceOutput(output))
    }

    override def visitIdentity[I : OP](expr: Expr.Identity[I, OP]): PO <:< I => ExprResult[PO, I, I, OP] = {
      implicit evPOisI =>
        val input = evPOisI(state.output)
        expr.debugging.attach(state.withBoth(input, input))
        ExprResult.Identity(expr, state.swapAndReplaceOutput(input))
    }

    override def visitMapEvery[C[_] : Functor, A, B](
      expr: Expr.MapEvery[C, A, B, OP],
    )(implicit
      opO: OP[C[B]],
    ): PO <:< C[A] => ExprResult[PO, C[A], C[B], OP] = { implicit evPOisI =>
      val input = evPOisI(state.output)
      val results = input.map { e =>
        expr.mapExpr.visit(withState(state.withOutput(e)))(implicitly)
      }
      val combinedOutput = results.map(_.state.output)
      val newState = state.swapAndReplaceOutput(combinedOutput)
      expr.debugging.attach(newState)
      ExprResult.MapEvery(expr, newState, results)
    }

    override def visitNot[I, O : Negation : OP](expr: Expr.Not[I, O, OP]): PO <:< I => ExprResult[PO, I, O, OP] = {
      implicit evPOisI =>
        val booleanResult = expr.innerExpr.visit(withState(state))(implicitly)
        val output = booleanResult.state.output
        val negatedOutput = Negation[O].negation(output)
        val newState = state.swapAndReplaceOutput(negatedOutput)
        val debugState = newState.mapInput((_, booleanResult.state.output))
        expr.debugging.attach(debugState)
        ExprResult.Not(expr, newState, booleanResult)
    }

    override def visitOr[I](
      expr: Expr.Or[I, OP],
    )(implicit
      evO: OP[Boolean],
    ): PO <:< I => ExprResult[PO, I, Boolean, OP] = { implicit evPOisI =>
      val left = expr.leftExpr.visit(this)(implicitly)
      val right = expr.rightExpr.visit(this)(implicitly)
      val output = left.state.output || right.state.output
      // TODO: Do justification here
      val newState = state.swapAndReplaceOutput(output)
      expr.debugging.attach(newState)
      ExprResult.Or(expr, newState, left, right)
    }

    override def visitValuesOfType[T, O](
      expr: Expr.ValuesOfType[T, O, OP],
    )(implicit
      opTs: OP[Seq[O]],
    ): PO <:< Any => ExprResult[PO, Any, Seq[O], OP] = { implicit evPOisI =>
      val matchingFacts = state.factTable.getSortedSeq(expr.factTypeSet)
      val matchingValues = matchingFacts.map(expr.transform)
      val newState = state.swapAndReplaceOutput(matchingValues)
      expr.debugging.attach(newState)
      ExprResult.ValuesOfType(expr, newState)
    }

    override def visitWithinWindow[I, O](
      expr: Expr.WithinWindow[I, O, OP],
    )(implicit
      opB: OP[Boolean],
    ): PO <:< I => ExprResult[PO, I, Boolean, OP] = { implicit evPOisI =>
      val valueResult = expr.valueExpr.visit(this)(implicitly)
      val windowResult = expr.windowExpr.visit(this)(implicitly)
      val comparisonResult = Window.contains(windowResult.state.output, valueResult.state.output)
      val newState = state.swapAndReplaceOutput(comparisonResult)
      expr.debugging.attach(newState)
      ExprResult.WithinWindow(expr, newState, valueResult, windowResult)
    }
  }
}
