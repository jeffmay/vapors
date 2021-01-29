package com.rallyhealth.vapors.factfilter.evaluator

import cats.{Eval, Foldable, Functor, Semigroupal}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsSimpleOutputFn.GenSimpleOutput

/**
  * Interprets an [[Expr]] as a function from [[ExprInput]] to a tuple of [[ExprOutput]] and a list of captured params.
  *
  * @see [[GenSimpleOutput]] for the interpreted function return type.
  *
  * This is necessary to define a [[Functor]] and [[Semigroupal]] for an expression, so that we can recursively map
  * over the whole [[com.rallyhealth.vapors.core.algebra.NonEmptyExprHList]] to get an [[shapeless.HList]] result.
  *
  * Since the [[InterpretExprAsResultFn]] return type is dependent on the input [[Expr]], it is impossible to define
  * these instances. By simplifying to the more generic return type of [[GenSimpleOutput]] we are able to leverage
  * the standard function, tuple, and list instances from cats over the well defined type constructor of [[ExprOutput]].
  */
class InterpretExprAsSimpleOutputFn[F[_] : Foldable, V, P]
  extends VisitGenericExprWithProxyFn[F, V, P, GenSimpleOutput[*, P]] {

  override protected def visitGeneric[M[_] : Foldable, U, R](
    expr: Expr[M, U, R, P],
    input: ExprInput[M, U],
  ): GenSimpleOutput[R, P] = {
    val result = InterpretExprAsResultFn(expr)(input)
    (result.output, result.param :: Nil)
  }
}

object InterpretExprAsSimpleOutputFn {
  type GenSimpleOutput[R, P] = (ExprOutput[R], List[Eval[P]])

  class SimpleOutputFnFunctorBuilder[F[_], V, P] {
    type SimpleOutput[R] = GenSimpleOutput[R, P]
    type SimpleOutputFn[R] = ExprInput[F, V] => SimpleOutput[R]

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
        (ExprOutput((a.value, b.value), a.evidence & b.evidence), aParams ::: bParams)
      }
    }
  }
}
