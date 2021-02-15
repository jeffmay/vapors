package com.rallyhealth.vapors.core.dsl

import cats.Foldable
import com.rallyhealth.vapors.core.algebra.Expr

trait WithOutputSyntax {

  implicit def thenChainValue[V, R, P](expr: Expr[V, R, P]): WithOutputValueExprBuilder[V, R, P] =
    new WithOutputValueExprBuilder(expr)

  implicit def thenChainFoldable[V, M[_], R, P](expr: Expr[V, M[R], P]): WithOutputFoldableExprBuilder[V, M, R, P] =
    new WithOutputFoldableExprBuilder(expr)

}

final class WithOutputValueExprBuilder[V, R, P](private val expr: Expr[V, R, P]) extends AnyVal {

  def withOutputValue: ValExprBuilder[V, R, P] = new ValExprBuilder(expr)
}

final class WithOutputFoldableExprBuilder[V, M[_], R, P](private val expr: Expr[V, M[R], P]) extends AnyVal {

  def withOutputFoldable(
    implicit
    foldableM: Foldable[M],
  ): FoldableExprBuilder[V, M, R, P] = new FoldableExprBuilder(expr)
}
