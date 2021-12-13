package com.rallyhealth.vapors.v1

package dsl

import algebra._
import cats.{catsInstancesForId, Foldable, Functor}
import data.FactTypeSet
import lens.VariantLens
import logic.Logic

trait UnwrappedBuildExprDsl extends BuildExprDsl with UnwrappedImplicits with UnwrappedDslTypes {

  override protected implicit final def boolLogic: Logic[W, Boolean, OP] = Logic.bool

  override protected implicit final def windowComparable: WindowComparable[W, OP] = WindowComparable.identity

  override protected implicit final def extract: Extract[W] = Extract.identity

  override protected implicit final def functor: Functor[W] = catsInstancesForId

  override protected implicit final def wrapConst: WrapConst[W] = WrapConst.identity

  override protected implicit final def selectElement: WrapSelected[W, OP] = WrapSelected.unwrapped

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

  override implicit final def in[I, T](expr: I ~:> T): SelectIdExprBuilder[I, T] = new SelectIdExprBuilder(expr)

  final class SelectIdExprBuilder[-I, A](inputExpr: I ~:> A) extends SelectExprBuilder[I, A] {

    override def get[B : Wrappable, O](
      selector: VariantLens.FromTo[A, B],
    )(implicit
      sot: SelectOutputType.Aux[W, A, B, O],
      opO: OP[O],
    ): Expr.Select[I, W, A, B, O, OP] = {
      val lens = selector(VariantLens.id[A])
      Expr.Select[I, W, A, B, O, OP](inputExpr, lens, sot.wrapSelected(_, lens.path, _))
    }

    override def getAs[C[_]]: GetAsUnwrapped[I, A, C, OP] = new GetAsUnwrapped(inputExpr)
  }

  override implicit final def hk[I, C[_], A](expr: I ~:> C[A])(implicit ne: NotEmpty[C, A]): HkIdExprBuilder[I, C, A] =
    new HkIdExprBuilder(expr)

  override final type SpecificHkExprBuilder[-I, C[_], A] = HkIdExprBuilder[I, C, A]

  final class HkIdExprBuilder[-I, C[_], A](inputExpr: I ~:> C[A]) extends HkExprBuilder(inputExpr) {

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

sealed trait UnwrappedImplicits extends WrapImplicits with LowPriorityUnwrappedImplicits {

  implicit def selectFunctor[C[_] : Functor, I : OP, O : OP](
    implicit
    aot: SelectOutputType[W, I, O],
  ): SelectOutputType.Aux[W, I, C[O], C[aot.Out]] = defn.selectFunctor[C, I, O](aot)

  implicit def constFunctor[C[_] : Functor, O : OP](
    implicit
    aot: ConstOutputType[W, O],
  ): ConstOutputType.Aux[W, C[O], C[aot.Out]] = defn.constFunctor[C, O](aot)

}

sealed trait LowPriorityUnwrappedImplicits extends LowPriorityWrapImplicits with UnwrappedDslTypes {

  implicit def selectId[I : OP, O : OP]: SelectOutputType.Aux[W, I, O, O] = defn.selectId

  implicit def constId[O : OP]: ConstOutputType.Aux[W, O, O] = defn.constId
}
