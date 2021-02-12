package com.rallyhealth.vapors.core.dsl

import cats.{Align, FunctorFilter}
import com.rallyhealth.vapors.core.algebra.{CaptureP, Expr, ExprConverter, NonEmptyExprHList}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

trait WrapEachExprSyntax {

  def wrapEach[F[_], V, M[_], E1, E2, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: NonEmptyExprHList.tailK(e2))

  def wrapEach[F[_], V, M[_], E1, E2, E3, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: NonEmptyExprHList.tailK(e3))

  def wrapEach[F[_], V, M[_], E1, E2, E3, E4, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: NonEmptyExprHList.tailK(e4))

  def wrapEach[F[_], V, M[_], E1, E2, E3, E4, E5, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
    e5: Expr[F, V, M[E5], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: E5 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: NonEmptyExprHList.tailK(e5))

  def wrapEach[F[_], V, M[_], E1, E2, E3, E4, E5, E6, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
    e5: Expr[F, V, M[E5], P],
    e6: Expr[F, V, M[E6], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: NonEmptyExprHList.tailK(e6))

  def wrapEach[F[_], V, M[_], E1, E2, E3, E4, E5, E6, E7, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
    e5: Expr[F, V, M[E5], P],
    e6: Expr[F, V, M[E6], P],
    e7: Expr[F, V, M[E7], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: NonEmptyExprHList.tailK(e7))

  def wrapEach[F[_], V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
    e5: Expr[F, V, M[E5], P],
    e6: Expr[F, V, M[E6], P],
    e7: Expr[F, V, M[E7], P],
    e8: Expr[F, V, M[E8], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: NonEmptyExprHList.tailK(e8))

  def wrapEach[F[_], V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, E9, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
    e5: Expr[F, V, M[E5], P],
    e6: Expr[F, V, M[E6], P],
    e7: Expr[F, V, M[E7], P],
    e8: Expr[F, V, M[E8], P],
    e9: Expr[F, V, M[E9], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: NonEmptyExprHList.tailK(e9))

  def wrapEach[F[_], V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
    e5: Expr[F, V, M[E5], P],
    e6: Expr[F, V, M[E6], P],
    e7: Expr[F, V, M[E7], P],
    e8: Expr[F, V, M[E8], P],
    e9: Expr[F, V, M[E9], P],
    e10: Expr[F, V, M[E10], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: HNil, P] =
    new WrapEachExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: NonEmptyExprHList.tailK(e10))

  def wrapEach[F[_], V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
    e5: Expr[F, V, M[E5], P],
    e6: Expr[F, V, M[E6], P],
    e7: Expr[F, V, M[E7], P],
    e8: Expr[F, V, M[E8], P],
    e9: Expr[F, V, M[E9], P],
    e10: Expr[F, V, M[E10], P],
    e11: Expr[F, V, M[E11], P],
  ): WrapEachExprHListWrapper[F, V, M, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: E11 :: HNil, P] =
    new WrapEachExprHListWrapper(
      e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: e10 :: NonEmptyExprHList.tailK(e11),
    )

  def wrapEach[F[_], V, M[_], E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, P](
    e1: Expr[F, V, M[E1], P],
    e2: Expr[F, V, M[E2], P],
    e3: Expr[F, V, M[E3], P],
    e4: Expr[F, V, M[E4], P],
    e5: Expr[F, V, M[E5], P],
    e6: Expr[F, V, M[E6], P],
    e7: Expr[F, V, M[E7], P],
    e8: Expr[F, V, M[E8], P],
    e9: Expr[F, V, M[E9], P],
    e10: Expr[F, V, M[E10], P],
    e11: Expr[F, V, M[E11], P],
    e12: Expr[F, V, M[E12], P],
  ): WrapEachExprHListWrapper[
    F,
    V,
    M,
    E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: E11 :: E12 :: HNil,
    P,
  ] =
    new WrapEachExprHListWrapper(
      e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: e10 :: e11 :: NonEmptyExprHList.tailK(e12),
    )
}

final class WrapEachExprHListWrapper[F[_], V, M[_], L <: HList, P](
  private val exprHList: NonEmptyExprHList[F, V, M, L, P],
) extends AnyVal {

  def zippedToShortest: ZippedToShortestExprWrapper[F, V, M, L, P] = new ZippedToShortestExprWrapper(exprHList)
}

final class ZippedToShortestExprWrapper[F[_], V, M[_], L <: HList, P](
  private val exprHList: NonEmptyExprHList[F, V, M, L, P],
) {

  // TODO: Maybe a good way to make these methods generic by carrying typeclasses and the function to apply?
  //       This would avoid the naming repetition and capture / converter logic more consistent.

  def asTuple[T](
    implicit
    tupler: Tupler.Aux[L, T],
    alignM: Align[M],
    filterM: FunctorFilter[M],
    captureAllResults: CaptureP[F, V, M[T], P],
  ): Expr.ZipOutput[F, V, M, L, T, P] =
    Expr.ZipOutput(
      exprHList,
      ExprConverter.asTuple,
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
      ExprConverter.asHListIdentity,
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
      ExprConverter.asProductType,
      captureAllResults,
    )
}
