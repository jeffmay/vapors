package com.rallyhealth.vapors.core.dsl

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.algebra.{CaptureP, ConditionBranch, Expr}

final class WhenExprBuilder[F[_], V, P](private val whenExpr: CondExpr[F, V, P]) extends AnyVal {

  def thenReturn[R](thenExpr: Expr[F, V, R, P]): WhenElseExprBuilder[F, V, R, P] =
    new WhenElseExprBuilder(NonEmptyList.of(ConditionBranch(whenExpr, thenExpr)))
}

final class WhenElifExprBuilder[F[_], V, R, P](
  private val t: (CondExpr[F, V, P], NonEmptyList[ConditionBranch[F, V, R, P]]),
) extends AnyVal {

  def thenReturn(thenExpr: Expr[F, V, R, P]): WhenElseExprBuilder[F, V, R, P] =
    new WhenElseExprBuilder(ConditionBranch(t._1, thenExpr) :: t._2)
}

final class WhenElseExprBuilder[F[_], V, R, P](private val branches: NonEmptyList[ConditionBranch[F, V, R, P]])
  extends AnyVal {

  def elseReturn(
    elseExpr: Expr[F, V, R, P],
  )(implicit
    captureResult: CaptureP[F, V, R, P],
  ): Expr.When[F, V, R, P] =
    Expr.When(branches.reverse, elseExpr, captureResult)

  def elif(elifExpr: CondExpr[F, V, P]): WhenElifExprBuilder[F, V, R, P] = new WhenElifExprBuilder((elifExpr, branches))
}
