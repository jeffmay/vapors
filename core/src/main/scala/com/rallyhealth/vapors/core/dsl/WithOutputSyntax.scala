package com.rallyhealth.vapors.core.dsl

import cats.{Foldable, Id}
import com.rallyhealth.vapors.core.algebra.Expr

trait WithOutputSyntax {

  implicit def thenChainValue[V, R, P](expr: Expr[Id, V, R, P]): WithOutputValueExprBuilder[V, R, P] =
    new WithOutputValueExprBuilder(expr)

  implicit def thenChainFoldable[F[_], V, M[_], R, P](
    expr: Expr[F, V, M[R], P],
  ): WithOutputFoldableExprBuilder[F, V, M, R, P] =
    new WithOutputFoldableExprBuilder(expr)

}

final class WithOutputValueExprBuilder[V, R, P](private val expr: Expr[Id, V, R, P]) extends AnyVal {

  def withOutputValue: ValExprBuilder[V, R, P] = new ValExprBuilder(expr)
}

final class WithOutputFoldableExprBuilder[F[_], V, M[_], R, P](private val expr: Expr[F, V, M[R], P]) extends AnyVal {

  def withOutputFoldable(
    implicit
    foldableF: Foldable[F],
    foldableM: Foldable[M],
  ): FoldableExprBuilder[F, V, M, R, P] = new FoldableExprBuilder(expr)
}
