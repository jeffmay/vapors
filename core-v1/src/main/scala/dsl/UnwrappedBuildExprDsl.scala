package com.rallyhealth.vapors.v1

package dsl

import algebra._
import cats.{Foldable, Functor, Traverse}
import data.FactTypeSet
import lens.VariantLens
import logic.Logic
import shapeless.<:!<

trait UnwrappedBuildExprDsl extends BuildExprDsl with UnwrappedImplicits with UnwrappedDslTypes {

  override protected implicit final def boolLogic: Logic[W, Boolean, OP] = Logic.bool

  override protected implicit final def windowComparable: WindowComparable[W, OP] = WindowComparable.identity

  override protected implicit final def extract: Extract[W] = Extract.identity

  override protected implicit final def wrapConst: WrapConst[W, OP] = WrapConst.unwrapped

  override protected implicit final def wrapSelected: WrapSelected[W, OP] = WrapSelected.unwrapped

  override protected final val defn: WrapDefinitions[W, OP] = new WrapDefinitions[W, OP]

  // TODO: Should this be visible outside this trait?
  protected def shortCircuit: Boolean = true

  override final def ident[I](implicit opI: OP[I]): Expr.Identity[I, OP] = Expr.Identity()

  override final def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opTs: OP[Seq[T]],
  ): Expr.ValuesOfType[T, T, OP] =
    Expr.ValuesOfType(factTypeSet, _.value)

  override implicit final def const[A](
    value: A,
  )(implicit
    constType: ConstOutputType[W, A],
  ): ConstExprBuilder[constType.Out, OP] =
    new ConstExprBuilder(constType.wrapConst(value))

  override implicit final def in[I, T](expr: I ~:> T): UnwrappedSelectExprBuilder[I, T] =
    new UnwrappedSelectExprBuilder(expr)

  final class UnwrappedSelectExprBuilder[-I, A](inputExpr: I ~:> A) extends SelectExprBuilder[I, A] {

    override def get[B : Wrappable, O](
      selector: VariantLens.FromTo[A, B],
    )(implicit
      sot: SelectOutputType.Aux[W, A, B, O],
      opO: OP[O],
    ): Expr.Select[I, A, B, O, OP] = {
      val lens = selector(VariantLens.id[A])
      Expr.Select[I, A, B, O, OP](inputExpr, lens, sot.wrapSelected(_, lens.path, _))
    }

    override def getAs[C[_]]: GetAsUnwrapped[I, A, C, OP] = new GetAsUnwrapped(inputExpr)
  }

  override implicit final def hk[I, C[_], A](
    expr: I ~:> C[A],
  )(implicit
    ne: NotEmpty[C, A],
  ): UnwrappedHkExprBuilder[I, C, A] =
    new UnwrappedHkExprBuilder(expr)

  override final type SpecificHkExprBuilder[-I, C[_], A] = UnwrappedHkExprBuilder[I, C, A]

  final class UnwrappedHkExprBuilder[-I, C[_], A](inputExpr: I ~:> C[A]) extends HkExprBuilder(inputExpr) {

    override def headOption(
      implicit
      foldableC: Foldable[C],
      opA: OP[A],
      opO: OP[Option[A]],
    ): Expr.Select[I, C[A], Option[A], Option[A], OP] = {
      val lens = VariantLens.id[C[A]].at(0)
      Expr.Select(inputExpr, lens, (_, opt) => opt)
    }

    override def exists(
      conditionExprBuilder: A =~:> Boolean,
    )(implicit
      opO: OP[C[A]],
      opA: OP[A],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): AndThen[I, C[A], Boolean] =
      inputExpr.andThen(
        Expr.Exists[C, A, Boolean, OP](conditionExprBuilder(ident), _ => true, _ => false, shortCircuit),
      )

    override def forall(
      conditionExprBuilder: A =~:> Boolean,
    )(implicit
      opO: OP[C[A]],
      opA: OP[A],
      opB: OP[Boolean],
      foldC: Foldable[C],
    ): AndThen[I, C[A], Boolean] =
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
    ): AndThen[I, C[A], C[B]] =
      inputExpr.andThen(Expr.MapEvery[C, A, B, OP](mapExprBuilder(ident)))
  }
}

sealed trait UnwrappedImplicits extends MidPriorityUnwrappedImplicits with WrapImplicits {

  override implicit final def constFunctor[C[_] : Functor, O : OP](
    implicit
    cot: ConstOutputType[W, O],
  ): ConstOutputType.Aux[W, C[O], C[cot.Out]] = defn.constFunctor[C, O](cot)

  override implicit final def selectOption[I : OP, O : OP](
    implicit
    sot: SelectOutputType[W, I, O],
  ): SelectOutputType.Aux[W, I, Option[O], Option[sot.Out]] = defn.selectOption(sot)

}

sealed trait MidPriorityUnwrappedImplicits extends LowPriorityUnwrappedImplicits with MidPriorityWrapImplicits {

  override implicit final def selectTraverse[C[_] : Traverse, I : OP, O : OP](
    implicit
    sot: SelectOutputType[W, I, O],
    nt: C[O] <:!< Product,
  ): SelectOutputType.Aux[W, I, C[O], C[sot.Out]] = defn.selectTraverse(sot)
}

sealed trait LowPriorityUnwrappedImplicits extends LowPriorityWrapImplicits with UnwrappedDslTypes {

  override implicit final def selectId[I : OP, O : OP]: SelectOutputType.Aux[W, I, O, O] = defn.selectId

  override implicit final def constId[O : OP]: ConstOutputType.Aux[W, O, O] = defn.constId
}
