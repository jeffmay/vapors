package com.rallyhealth

package vapors.v1.engine

import vapors.v1.algebra.{Expr, ExprResult}
import vapors.v1.data.ExprState

import cats.Foldable

object InterpretExprAsFn {

  final class Visitor[PO](state: ExprState[Any, PO])
    extends Expr.Visitor[Lambda[(`-I`, `+O`) => I => ExprResult[PO, I, O]]] {

    import cats.implicits._

    override def visitCombine[I, LO, RO, LI, RI, O](
      expr: Expr.Combine[I, LO, RO, LI, RI, O],
    )(implicit
      evL: LO <:< LI,
      evR: RO <:< RI,
    ): I => ExprResult[PO, I, O] = { input =>
      val subState = state.swapAndReplaceOutput(input)
      val subVisitor = new Visitor(subState)
      val left = expr.leftExpr.visit(subVisitor)(input)
      val right = expr.rightExpr.visit(subVisitor)(input)
      val output = expr.operation(left.state.output, right.state.output)
      ExprResult.Combine(expr, subState.withOutput(output))
    }

    override def visitConst[O](expr: Expr.Const[O]): Any => ExprResult[PO, Any, O] = { _ =>
      ExprResult.Const(expr, state.swapAndReplaceOutput(expr.value))
    }

    override def visitExists[I, C[_] : Foldable, E](expr: Expr.Exists[I, C, E]): I => ExprResult[PO, I, Boolean] = {
      input =>
        val inputResult = expr.inputExpr.visit(this)(input)
        // TODO: Apply justification logic here
        val output = inputResult.state.output.exists { e =>
          val conditionState = new Visitor(state.withOutput(e))
          expr.conditionExpr.visit(conditionState)(e).state.output
        }
        ExprResult.Exists(expr, state.swapAndReplaceOutput(output))
    }

    override def visitForAll[I, C[_] : Foldable, E](expr: Expr.ForAll[I, C, E]): I => ExprResult[PO, I, Boolean] = {
      input =>
        val inputResult = expr.inputExpr.visit(this)(input)
        // TODO: Apply justification logic here
        val output = inputResult.state.output.forall { e =>
          val conditionVisitor = new Visitor(state.withOutput(e))
          expr.conditionExpr.visit(conditionVisitor)(e).state.output
        }
        ExprResult.ForAll(expr, state.swapAndReplaceOutput(output))
    }

    override def visitIdentity[I, O](
      expr: Expr.Identity[I, O],
    )(implicit
      ev: I <:< O,
    ): I => ExprResult[PO, I, O] = { input =>
      ExprResult.Identity(state.swapAndReplaceOutput(input))
    }

    override def visitWithFactValues[T, O](expr: Expr.WithFactValues[T, O]): Any => ExprResult[PO, Any, O] = { _ =>
      val matchingFactValues = state.factTable.getSortedSeq(expr.factTypeSet).map(_.value)
      val subState = state.withOutput(matchingFactValues)
      val result = expr.outputExpr.visit(new Visitor(subState))(subState.output)
      ExprResult.WithFactValues(expr, state.swapAndReplaceOutput(result.state.output))
    }

  }
}
