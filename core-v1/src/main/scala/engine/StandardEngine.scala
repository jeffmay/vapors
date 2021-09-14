package com.rallyhealth.vapors.v1

package engine

import algebra.{Expr, ExprResult}
import data.ExprState

import cats.Foldable

object StandardEngine {

  @inline def apply[OP[_]]: Applied[OP] = new Applied[OP]

  final class Applied[OP[_]](private val dummy: Boolean = true) extends AnyVal {
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

    override def visitExists[C[_] : Foldable, E](
      expr: Expr.Exists[C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): PO <:< C[E] => ExprResult[PO, C[E], Boolean, OP] = { implicit evPOisI =>
      // TODO: Apply justification logic here
      val output = evPOisI(state.output).exists { e =>
        val conditionState = withState(state.withOutput(e))
        expr.conditionExpr.visit(conditionState)(implicitly).state.output
      }
      val newState = state.swapAndReplaceOutput(output)
      expr.debugging.attach(newState)
      ExprResult.Exists(expr, newState)
    }

    override def visitForAll[C[_] : Foldable, E](
      expr: Expr.ForAll[C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): PO <:< C[E] => ExprResult[PO, C[E], Boolean, OP] = { implicit evPOisI =>
      // TODO: Apply justification logic here
      val output = evPOisI(state.output).forall { e =>
        val conditionVisitor = withState(state.withOutput(e))
        expr.conditionExpr.visit(conditionVisitor)(implicitly).state.output
      }
      ExprResult.ForAll(expr, state.swapAndReplaceOutput(output))
    }

    override def visitIdentity[I, O : OP](
      expr: Expr.Identity[I, O, OP],
    )(implicit
      evIisO: I <:< O,
    ): PO <:< I => ExprResult[PO, I, O, OP] = { implicit evPOisI =>
      val input = evPOisI(state.output)
      expr.debugging.attach(state.withBoth(input, input))
      ExprResult.Identity(expr, state.swapAndReplaceOutput(input))
    }

    override def visitValuesOfType[T](
      expr: Expr.ValuesOfType[T, OP],
    )(implicit
      opTs: OP[Seq[T]],
    ): PO <:< Any => ExprResult[PO, Any, Seq[T], OP] = { implicit evPOisI =>
      val matchingFactValues = state.factTable.getSortedSeq(expr.factTypeSet).map(_.value)
      val newState = state.swapAndReplaceOutput(matchingFactValues)
      expr.debugging.attach(newState)
      ExprResult.ValuesOfType(expr, newState)
    }
  }
}
