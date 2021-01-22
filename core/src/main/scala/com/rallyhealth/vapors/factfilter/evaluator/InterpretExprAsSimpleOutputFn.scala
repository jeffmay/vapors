package com.rallyhealth.vapors.factfilter.evaluator

import cats.{Eval, Foldable}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsResultFn.{Input, Output}

class InterpretExprAsSimpleOutputFn[F[_] : Foldable, V, P]
  extends VisitGenericExprWithProxy[F, V, P, Lambda[r => (Output[r], List[Eval[P]])]] {

  override protected def visitGeneric[M[_] : Foldable, U, R](
    expr: Expr[M, U, R, P],
    input: Input[M, U],
  ): (Output[R], List[Eval[P]]) = {
    val result = InterpretExprAsResultFn(expr)(input)
    (result.output, result.param :: Nil)
  }
}

object InterpretExprAsSimpleOutputFn {

  type OutputAndParam[F[_], V, R, P] = Input[F, V] => (Output[R], List[Eval[P]])
}
