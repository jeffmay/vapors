package com.rallyhealth.vapors.core.dsl

import cats.{Align, Functor, FunctorFilter, Semigroupal}
import com.rallyhealth.vapors.core.algebra.{CaptureP, Expr, NonEmptyExprHList}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

trait ZipExprSyntax {

  def zip[F[_], V, M[_] : Align : Functor, E1, E2, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
  ): ZipExprHListWrapper[F, V, M, E1 :: E2 :: HNil, P] =
    new ZipExprHListWrapper(e1 :: NonEmptyExprHList.tailK(e2))

  def zip[F[_], V, M[_] : Align : Functor, E1, E2, E3, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
  ): ZipExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: HNil, P] =
    new ZipExprHListWrapper(e1 :: e2 :: NonEmptyExprHList.tailK(e3))

  def zip[F[_], V, M[_] : Align : Functor, E1, E2, E3, E4, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
  ): ZipExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: HNil, P] =
    new ZipExprHListWrapper(e1 :: e2 :: e3 :: NonEmptyExprHList.tailK(e4))

  def zip[F[_], V, M[_] : Functor : Semigroupal, E1, E2, E3, E4, E5, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
    e5: Expr[F, V, M[E5], P],
  ): ZipExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: E5 :: HNil, P] =
    new ZipExprHListWrapper(e1 :: e2 :: e3 :: e4 :: NonEmptyExprHList.tailK(e5))
}

final class ZipExprHListWrapper[F[_], V, M[_], L <: HList, P](private val exprHList: NonEmptyExprHList[F, V, M, L, P])
  extends AnyVal {

  def asTuple[T](
    implicit
    tupler: Tupler.Aux[L, T],
    alignM: Align[M],
    filterM: FunctorFilter[M],
    captureAllResults: CaptureP[F, V, M[T], P],
  ): Expr.ZipOutput[F, V, M, L, T, P] =
    Expr.ZipOutput(
      exprHList,
      Expr.WrapOutput.asTuple,
      captureAllResults,
    )

  def asHList(
    implicit
    alignM: Align[M],
    filterM: FunctorFilter[M],
    captureAllResults: CaptureP[F, V, M[L], P],
  ): Expr.ZipOutput[F, V, M, L, L, P] =
    Expr.ZipOutput(
      exprHList,
      Expr.WrapOutput.asHListIdentity,
      captureAllResults,
    )

  def as[R](
    implicit
    gen: Generic.Aux[R, L],
    alignM: Align[M],
    filterM: FunctorFilter[M],
    captureAllResults: CaptureP[F, V, M[R], P],
  ): Expr.ZipOutput[F, V, M, L, R, P] =
    Expr.ZipOutput(
      exprHList,
      Expr.WrapOutput.asProductType,
      captureAllResults,
    )
}
