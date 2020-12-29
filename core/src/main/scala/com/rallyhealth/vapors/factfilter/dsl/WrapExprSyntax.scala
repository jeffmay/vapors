package com.rallyhealth.vapors.factfilter.dsl

import com.rallyhealth.vapors.core.algebra.{Expr, ExprLast, NonEmptyExprHList}
import shapeless.{::, Generic, HList, HNil}

trait WrapExprSyntax {

  def wrap[F[_], V, A, B, P](
    e1: Expr[F, V, A, P],
    e2: Expr[F, V, B, P],
  ): ExprHListWrapper[F, V, A :: B :: HNil, P] =
    new ExprHListWrapper(e1 :: ExprLast(e2))

  def wrap[F[_], V, A, B, C, P](
    e1: Expr[F, V, A, P],
    e2: Expr[F, V, B, P],
    e3: Expr[F, V, C, P],
  ): ExprHListWrapper[F, V, A :: B :: C :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: ExprLast(e3))

  def wrap[F[_], V, A, B, C, D, P](
    e1: Expr[F, V, A, P],
    e2: Expr[F, V, B, P],
    e3: Expr[F, V, C, P],
    e4: Expr[F, V, D, P],
  ): ExprHListWrapper[F, V, A :: B :: C :: D :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: ExprLast(e4))

  def wrap[F[_], V, A, B, C, D, E, P](
    e1: Expr[F, V, A, P],
    e2: Expr[F, V, B, P],
    e3: Expr[F, V, C, P],
    e4: Expr[F, V, D, P],
    e5: Expr[F, V, E, P],
  ): ExprHListWrapper[F, V, A :: B :: C :: D :: E :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: ExprLast(e5))

}

final class ExprHListWrapper[F[_], V, L <: HList, P](private val l: NonEmptyExprHList[F, V, L, P]) extends AnyVal {

  def as[R](
    implicit
    gen: Generic.Aux[R, L],
    captureResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.WrapOutput(l, gen, captureResult)
}
