package com.rallyhealth.vapors.factfilter.dsl

import com.rallyhealth.vapors.core.algebra.{Expr, ExprLast, NonEmptyExprHList}
import shapeless.ops.hlist.Tupler
import shapeless.{::, Generic, HList, HNil}

trait WrapExprSyntax {

  def wrap[F[_], V, E1, E2, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: HNil, P] =
    new ExprHListWrapper(e1 :: ExprLast(e2))

  def wrap[F[_], V, E1, E2, E3, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: ExprLast(e3))

  def wrap[F[_], V, E1, E2, E3, E4, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
    e4: Expr[F, V, E4, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: E4 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: ExprLast(e4))

  def wrap[F[_], V, E1, E2, E3, E4, E5, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
    e4: Expr[F, V, E4, P],
    e5: Expr[F, V, E5, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: E4 :: E5 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: ExprLast(e5))

  def wrap[F[_], V, E1, E2, E3, E4, E5, E6, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
    e4: Expr[F, V, E4, P],
    e5: Expr[F, V, E5, P],
    e6: Expr[F, V, E6, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: ExprLast(e6))

  def wrap[F[_], V, E1, E2, E3, E4, E5, E6, E7, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
    e4: Expr[F, V, E4, P],
    e5: Expr[F, V, E5, P],
    e6: Expr[F, V, E6, P],
    e7: Expr[F, V, E7, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: ExprLast(e7))

  def wrap[F[_], V, E1, E2, E3, E4, E5, E6, E7, E8, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
    e4: Expr[F, V, E4, P],
    e5: Expr[F, V, E5, P],
    e6: Expr[F, V, E6, P],
    e7: Expr[F, V, E7, P],
    e8: Expr[F, V, E8, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: ExprLast(e8))

  def wrap[F[_], V, E1, E2, E3, E4, E5, E6, E7, E8, E9, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
    e4: Expr[F, V, E4, P],
    e5: Expr[F, V, E5, P],
    e6: Expr[F, V, E6, P],
    e7: Expr[F, V, E7, P],
    e8: Expr[F, V, E8, P],
    e9: Expr[F, V, E9, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: ExprLast(e9))

  def wrap[F[_], V, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
    e4: Expr[F, V, E4, P],
    e5: Expr[F, V, E5, P],
    e6: Expr[F, V, E6, P],
    e7: Expr[F, V, E7, P],
    e8: Expr[F, V, E8, P],
    e9: Expr[F, V, E9, P],
    e10: Expr[F, V, E10, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: ExprLast(e10))

  def wrap[F[_], V, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
    e4: Expr[F, V, E4, P],
    e5: Expr[F, V, E5, P],
    e6: Expr[F, V, E6, P],
    e7: Expr[F, V, E7, P],
    e8: Expr[F, V, E8, P],
    e9: Expr[F, V, E9, P],
    e10: Expr[F, V, E10, P],
    e11: Expr[F, V, E11, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: E11 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: e10 :: ExprLast(e11))

  def wrap[F[_], V, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, P](
    e1: Expr[F, V, E1, P],
    e2: Expr[F, V, E2, P],
    e3: Expr[F, V, E3, P],
    e4: Expr[F, V, E4, P],
    e5: Expr[F, V, E5, P],
    e6: Expr[F, V, E6, P],
    e7: Expr[F, V, E7, P],
    e8: Expr[F, V, E8, P],
    e9: Expr[F, V, E9, P],
    e10: Expr[F, V, E10, P],
    e11: Expr[F, V, E11, P],
    e12: Expr[F, V, E12, P],
  ): ExprHListWrapper[F, V, E1 :: E2 :: E3 :: E4 :: E5 :: E6 :: E7 :: E8 :: E9 :: E10 :: E11 :: E12 :: HNil, P] =
    new ExprHListWrapper(e1 :: e2 :: e3 :: e4 :: e5 :: e6 :: e7 :: e8 :: e9 :: e10 :: e11 :: ExprLast(e12))

}

final class ExprHListWrapper[F[_], V, L <: HList, P](private val exprHList: NonEmptyExprHList[F, V, L, P])
  extends AnyVal {

  def as[R](
    implicit
    gen: Generic.Aux[R, L],
    captureResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.WrapOutput(exprHList, Expr.WrapOutput.asProductType, captureResult)

  def asHList(
    implicit
    captureResult: CaptureP[F, V, L, P],
  ): Expr[F, V, L, P] =
    Expr.WrapOutput(exprHList, Expr.WrapOutput.asHListIdentity, captureResult)

  def asTuple[T](
    implicit
    tupler: Tupler.Aux[L, T],
    captureResult: CaptureP[F, V, T, P],
  ): Expr[F, V, T, P] =
    Expr.WrapOutput(exprHList, Expr.WrapOutput.asTuple, captureResult)
}

final class GenericIdentity[R] extends Generic[R] {
  override type Repr = R
  override def to(repr: R): Repr = repr
  override def from(repr: Repr): R = repr
}

object GenericIdentity {
  def apply[R]: GenericIdentity[R] = new GenericIdentity[R]
}
