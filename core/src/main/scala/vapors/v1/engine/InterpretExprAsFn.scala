package com.rallyhealth

package vapors.v1.engine

import vapors.v1.algebra.{Expr, ExprResult}
import vapors.v1.data.ExprState

import cats.Foldable

object InterpretExprAsFn {

  final object Visitor {

    @inline def apply[OP[_]]: Using[OP] = new Using[OP]

    final class Using[OP[_]](private val dummy: Boolean = true) extends AnyVal {
      def apply[PO](state: ExprState[Any, PO]): Visitor[PO, OP] = new Visitor(state)
    }
  }

  /**
    *
    * @note In Scala 3, this could just become a context function, which would simplify all the places
    *       where I am using `implicitly`.
    */
  class Visitor[PO, OP[_]](state: ExprState[Any, PO])
    extends Expr.Visitor[Lambda[(`-I`, `+O`) => PO <:< I => ExprResult[PO, I, O, OP]], OP] {

    import cats.implicits._

    override def visitCombine[I, LO : OP, RO : OP, LI, RI, O : OP](
      expr: Expr.Combine[I, LO, RO, LI, RI, O, OP],
    )(implicit
      evL: LO <:< LI,
      evR: RO <:< RI,
    ): PO <:< I => ExprResult[PO, I, O, OP] = { implicit evI =>
      val left = expr.leftExpr.visit(this)(implicitly)
      val right = expr.rightExpr.visit(this)(implicitly)
      val output = expr.operation(left.state.output, right.state.output)
      ExprResult.Combine(expr, state.swapAndReplaceOutput(output), left, right)
    }

    override def visitConst[O : OP](expr: Expr.Const[O, OP]): PO <:< Any => ExprResult[PO, Any, O, OP] = { _ =>
      ExprResult.Const(expr, state.swapAndReplaceOutput(expr.value))
    }

    override def visitExists[I, C[_] : Foldable, E](
      expr: Expr.Exists[I, C, E, OP],
    )(implicit
      opC: OP[C[E]],
      opO: OP[Boolean],
    ): PO <:< I => ExprResult[PO, I, Boolean, OP] = { implicit evI =>
      val inputResult = expr.inputExpr.visit(this)(implicitly)
      // TODO: Apply justification logic here
      val output = inputResult.state.output.exists { e =>
        val conditionState = Visitor[OP](state.withOutput(e))
        expr.conditionExpr.visit(conditionState)(implicitly).state.output
      }
      ExprResult.Exists(expr, state.swapAndReplaceOutput(output), inputResult)
    }

    override def visitForAll[I, C[_] : Foldable, E](
      expr: Expr.ForAll[I, C, E, OP],
    )(implicit
      opC: OP[C[E]],
      opO: OP[Boolean],
    ): PO <:< I => ExprResult[PO, I, Boolean, OP] = { implicit evI =>
      val inputResult = expr.inputExpr.visit(this)(implicitly)
      // TODO: Apply justification logic here
      val output = inputResult.state.output.forall { e =>
        val conditionVisitor = Visitor[OP](state.withOutput(e))
        expr.conditionExpr.visit(conditionVisitor)(implicitly).state.output
      }
      ExprResult.ForAll(expr, state.swapAndReplaceOutput(output))
    }

    override def visitIdentity[I, O : OP](
      expr: Expr.Identity[I, O, OP],
    )(implicit
      ev: I <:< O,
    ): PO <:< I => ExprResult[PO, I, O, OP] = { implicit evI =>
      val input = evI(state.output)
      ExprResult.Identity(state.swapAndReplaceOutput(input))
    }

    override def visitWithFactValues[T, O : OP](
      expr: Expr.WithFactValues[T, O, OP],
    ): PO <:< Any => ExprResult[PO, Any, O, OP] = { implicit evI =>
      val matchingFactValues = state.factTable.getSortedSeq(expr.factTypeSet).map(_.value)
      val subState = state.withOutput[Seq[T]](matchingFactValues)
      val subVisitor = Visitor[OP](subState)
      val result = expr.outputExpr.visit(subVisitor)(implicitly)
      ExprResult.WithFactValues(expr, state.swapAndReplaceOutput(result.state.output))
    }
  }
}
