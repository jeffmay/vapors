package com.rallyhealth.vapors.factfilter.evaluator

import cats.{Eval, Foldable, Functor, Semigroupal}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsResultFn.{Input, Output}

class InterpretExprAsSimpleOutputFn[F[_] : Foldable, V, P]
  extends VisitGenericExprWithProxyFn[F, V, P, InterpretExprAsSimpleOutputFn.GenSimpleOutput[*, P]] {
  import InterpretExprAsSimpleOutputFn.GenSimpleOutput

  override protected def visitGeneric[M[_] : Foldable, U, R](
    expr: Expr[M, U, R, P],
    input: Input[M, U],
  ): GenSimpleOutput[R, P] = {
    val result = InterpretExprAsResultFn(expr)(input)
    (result.output, result.param :: Nil)
  }
}

object InterpretExprAsSimpleOutputFn {
  type GenSimpleOutput[R, P] = (Output[R], List[Eval[P]])

  class SimpleOutputFnFunctorBuilder[F[_], V, P] {
    type SimpleOutput[R] = GenSimpleOutput[R, P]
    type SimpleOutputFn[R] = Input[F, V] => SimpleOutput[R]

    implicit object SimpleOutputFunctor extends Functor[SimpleOutputFn] with Semigroupal[SimpleOutputFn] {
      override def map[A, B](fa: SimpleOutputFn[A])(f: A => B): SimpleOutputFn[B] = fa.andThen {
        case (o, params) => (o.copy(value = f(o.value)), params)
      }

      override def product[A, B](
        fa: SimpleOutputFn[A],
        fb: SimpleOutputFn[B],
      ): SimpleOutputFn[(A, B)] = { input =>
        val (a, aParams) = fa(input)
        val (b, bParams) = fb(input)
        // TODO: Product type should remove evidence if any arg has no evidence
        (Output((a.value, b.value), a.evidence ++ b.evidence), aParams ::: bParams)
      }
    }
  }
}
