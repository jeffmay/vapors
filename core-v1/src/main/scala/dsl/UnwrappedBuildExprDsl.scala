package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data.FactTypeSet
import lens.VariantLens
import logic.Logic

import cats.{catsInstancesForId, Foldable, Functor, FunctorFilter}
import shapeless.{Generic, HList}

trait UnwrappedBuildExprDsl extends BuildExprDsl with UnwrappedDslTypes {

  override protected implicit final def boolLogic: Logic[W, Boolean, OP] = Logic.bool

  override protected implicit final def windowComparable: WindowComparable[W, OP] = WindowComparable.identity

  override protected implicit final def extract: Extract[W] = Extract.identity

  override protected implicit final def functor: Functor[W] = catsInstancesForId

  override protected implicit final def wrapConst: WrapConst[W] = WrapConst.identity

  // TODO: Should this be visible outside this trait?
  protected def shortCircuit: Boolean = true

  override final def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opTs: OP[Seq[T]],
  ): Expr.ValuesOfType[T, T, OP] =
    Expr.ValuesOfType(factTypeSet, _.value)

  override implicit final def in[I, T](expr: I ~:> T): SelectIdExprBuilder[I, T] = new SelectIdExprBuilder(expr)

  override final type SpecificSelectExprBuilder[-I, T] = SelectIdExprBuilder[I, T]

  final class SelectIdExprBuilder[-I, T](override protected val inputExpr: I ~:> T) extends SelectExprBuilder[I, T] {

    override def get[O](selector: VariantLens.FromTo[T, O])(implicit opO: OP[O]): I ~:> O =
      inputExpr.selectWith(selector(VariantLens.id[T]))
  }

  override implicit def wrapHList[I, L <: HList](expr: I ~:> L): WrapHListIdExprBuilder[I, L] =
    new WrapHListIdExprBuilder(expr)

  override type SpecificWrapHListExprBuilder[-I, L <: HList] = WrapHListIdExprBuilder[I, L]

  final class WrapHListIdExprBuilder[-I, L <: HList](override protected val inputExpr: I ~:> L)
    extends WrapHListExprBuilder[I, L] {

    override def as[P](
      implicit
      gen: Generic.Aux[P, L],
      opL: OP[L],
      opP: OP[P],
    ): Ap[I, L, P] =
      Expr.AndThen(inputExpr, Expr.Convert(ExprConverter.asProductType))
  }

  override implicit final def hk[I, C[_], A](expr: I ~:> C[A]): HkIdExprBuilder[I, C, A] = new HkIdExprBuilder(expr)

  override final type SpecificHkExprBuilder[-I, C[_], A] = HkIdExprBuilder[I, C, A]

  final class HkIdExprBuilder[-I, C[_], A](override protected val inputExpr: I ~:> C[A])
    extends HkExprBuilder[I, C, A] {

    override def exists(
      conditionExprBuilder: A =~:> Boolean,
    )(implicit
      opO: OP[C[A]],
      opA: OP[A],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): Ap[I, C[A], Boolean] =
      inputExpr.andThen(
        Expr.Exists[C, A, Boolean, OP](conditionExprBuilder(ident), _ => true, _ => false, shortCircuit),
      )

    override def filter(
      conditionExprBuilder: A =~:> Boolean,
    )(implicit
      opO: OP[C[A]],
      opA: OP[A],
      opB: OP[Boolean],
      filterC: FunctorFilter[C],
    ): Ap[I, C[A], C[A]] =
      inputExpr.andThen(Expr.Filter(conditionExprBuilder(ident)))

    override def forall(
      conditionExprBuilder: A =~:> Boolean,
    )(implicit
      opO: OP[C[A]],
      opA: OP[A],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): Ap[I, C[A], Boolean] =
      inputExpr.andThen(
        Expr.ForAll[C, A, Boolean, OP](conditionExprBuilder(ident), _ => true, _ => false, shortCircuit),
      )

    override def map[B](
      mapExprBuilder: A =~:> B,
    )(implicit
      opI: OP[A],
      opA: OP[C[A]],
      opB: OP[C[B]],
      functorC: Functor[C],
    ): Ap[I, C[A], C[B]] =
      inputExpr.andThen(Expr.MapEvery[C, A, B, OP](mapExprBuilder(ident)))
  }
}
