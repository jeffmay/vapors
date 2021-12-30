package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data.{Extract, FactTypeSet}
import lens.VariantLens
import logic.Logic
import math.Power

import cats.data.NonEmptySeq
import cats.{FlatMap, Foldable, Functor, FunctorFilter, Reducible}
import shapeless.{Generic, HList, Nat}

trait UnwrappedBuildExprDsl
  extends BuildExprDsl
  with DefaultUnwrappedExprHListImplicits
  with DefaultUnwrappedOutputTypeImplicits
  with UnwrappedDslTypes {

  override protected implicit final def boolLogic: Logic[W, Boolean, OP] = Logic.bool

  override protected implicit final def windowComparable: WindowComparable[W, OP] = WindowComparable.identity

  override protected implicit final def extract: Extract[W] = Extract.identity

  override protected implicit final def wrapConst: WrapConst[W, OP] = WrapConst.unwrapped

  override protected implicit final def wrapSelected: WrapSelected[W, OP] = WrapSelected.unwrapped

  // TODO: Should this be visible outside this trait?
  protected final def shortCircuit: Boolean = true

  override final def ident[I](implicit opI: OP[I]): Expr.Identity[I, OP] = Expr.Identity()

  override final def seq[I, O](expressions: I ~:> O*)(implicit opO: OP[Seq[O]]): I ~:> Seq[O] =
    super.seq[I, O](expressions: _*)

  override final def some[I, O](expr: I ~:> O)(implicit opO: OP[Option[O]]): I ~:> Option[O] = super.some[I, O](expr)

  override final def none[O](implicit opO: OP[Option[O]]): Any ~:> Option[O] = super.none[O]

  override final def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opT: OP[T],
    opTs: OP[Seq[T]],
  ): Expr.ValuesOfType[T, T, OP] =
    Expr.ValuesOfType(factTypeSet, _.value)

  override final def pow[I, L, R](
    leftExpr: I ~:> L,
    rightExpr: I ~:> R,
  )(implicit
    opR: OP[R],
    pow: Power[L, R],
  ): CombineHolder[I, L, L, R, R, pow.Out, OP] =
    (leftExpr ^ rightExpr)(opR, pow)

  override final def when[I](condExpr: I ~:> Boolean): UnwrappedWhenBuilder[I] = new UnwrappedWhenBuilder(condExpr)

  final class UnwrappedWhenBuilder[-I](condExpr: I ~:> Boolean) extends WhenBuilder[I, Boolean](condExpr) {
    override def thenReturn[TI <: I, TO](thenExpr: TI ~:> TO): UnwrappedWhenElseBuilder[TI, TO] =
      new UnwrappedWhenElseBuilder(NonEmptySeq.of(Expr.ConditionBranch(condExpr, thenExpr)))
  }

  final class UnwrappedWhenElifBuilder[-I, +O](
    branches: NonEmptySeq[Expr.ConditionBranch[I, Boolean, O, OP]],
    nextCondExpr: I ~:> Boolean,
  ) extends WhenElifBuilder[I, Boolean, O](branches, nextCondExpr) {
    override def thenReturn[TI <: I, TO](thenExpr: TI ~:> TO): UnwrappedWhenElseBuilder[TI, TO] =
      new UnwrappedWhenElseBuilder(NonEmptySeq.of(Expr.ConditionBranch(nextCondExpr, thenExpr)))
  }

  final class UnwrappedWhenElseBuilder[-I, +O](branches: NonEmptySeq[Expr.ConditionBranch[I, Boolean, O, OP]])
    extends WhenElseBuilder(branches) {

    override def elif[CI <: I](nextCondExpr: CI ~:> W[Boolean]): UnwrappedWhenElifBuilder[CI, O] =
      new UnwrappedWhenElifBuilder(branches, nextCondExpr)

    override def elseReturn[EI <: I, EO >: O](
      elseExpr: EI ~:> EO,
    )(implicit
      opO: OP[EO],
    ): Expr.When[EI, Boolean, EO, OP] =
      Expr.When(branches, elseExpr)
  }

  override implicit final def const[A](
    value: A,
  )(implicit
    constType: ConstOutputType[W, A],
  ): ConstExprBuilder[constType.Out, OP] =
    new ConstExprBuilder(constType.wrapConst(value))

  override implicit final def in[I, T](expr: I ~:> T): UnwrappedSelectExprBuilder[I, T] =
    new UnwrappedSelectExprBuilder(expr)

  final class UnwrappedSelectExprBuilder[-I, A](inputExpr: I ~:> A) extends SelectExprBuilder(inputExpr) {

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

  override implicit final def xhlOps[I, WL <: HList](xhl: ExprHList[I, WL, OP]): UnwrappedExprHListOpsBuilder[I, WL] =
    new UnwrappedExprHListOpsBuilder(xhl)

  final class UnwrappedExprHListOpsBuilder[-I, WL <: HList](inputExprHList: ExprHList[I, WL, OP])
    extends ExprHListOpsBuilder(inputExprHList) {

    override def toHList[UL <: HList](
      implicit
      zip: ZipToShortest.Aux[W, WL, OP, UL],
      opO: OP[UL],
    ): I ~:> UL = {
      val expr = Expr.ZipToShortestHList(inputExprHList)
      expr: I ~:> UL // let the compiler prove that W[UL] == UL after computing the correct mapN implicit above
    }

    // IntelliJ highlights this as an error, but it compiles and it provides better type inference for the return type
    override def zipToShortest[C[+_], UL <: HList](
      implicit
      zip: ZipToShortest.Aux[C, WL, OP, UL],
      opO: OP[C[UL]],
    ): I ~:> C[UL] = {
      val expr = Expr.ZipToShortestHList[I, C, WL, UL, OP](inputExprHList)(zip, opO)
      expr: I ~:> C[UL] // let the compiler prove that C[W[UL]] == C[UL] after computing the correct mapN implicit above
    }
  }

  override implicit def fromHL[I, L <: HList](expr: I ~:> L): UnwrappedConvertHListExprBuilder[I, L] =
    new UnwrappedConvertHListExprBuilder(expr)

  final class UnwrappedConvertHListExprBuilder[-I, L <: HList](inputExpr: I ~:> L)
    extends ConvertHListExprBuilder(inputExpr) {

    override def as[P](
      implicit
      gen: Generic.Aux[P, L],
      opL: OP[L],
      opWL: OP[L],
      opP: OP[P],
      opWP: OP[P],
    ): AndThen[I, L, P] =
      inputExpr.andThen(Expr.Convert(ExprConverter.asProductType)(opWP))(opWP)
  }

  override implicit def sizeOf[I, C](inputExpr: I ~:> C): UnwrappedSizeOfExprBuilder[I, C] =
    new UnwrappedSizeOfExprBuilder(inputExpr)

  class UnwrappedSizeOfExprBuilder[-I, C](inputExpr: I ~:> C) extends SizeOfExprBuilder[I, C](inputExpr) {

    override def isEmpty(
      implicit
      sizeCompare: SizeComparable[C, Int, Boolean],
      opI: OP[Int],
      opWI: OP[Int],
      opWB: OP[Boolean],
    ): AndThen[I, C, Boolean] =
      inputExpr.andThen(Expr.SizeIs[C, Int, Boolean, OP](SizeComparison.===, Expr.Const(0)(opWI)))

    override def sizeIs: UnwrappedSizeIsBuilder[I, C] = new UnwrappedSizeIsBuilder(inputExpr)
  }

  override implicit final def hk[I, C[_], A](
    expr: I ~:> C[A],
  )(implicit
    ne: NotEmpty[C, A],
  ): UnwrappedHkExprBuilder[I, C, A] =
    new UnwrappedHkExprBuilder(expr)

  override final type SpecificHkExprBuilder[-I, C[_], A] = UnwrappedHkExprBuilder[I, C, A]

  final class UnwrappedHkExprBuilder[-I, C[_], A](inputExpr: I ~:> C[A]) extends HkExprBuilder(inputExpr) {

    override def head(
      implicit
      reducibleC: Reducible[C],
      opA: OP[A],
    ): Expr.Select[I, C[A], A, A, OP] = {
      val lens = VariantLens.id[C[A]].head
      Expr.Select(inputExpr, lens, (_, head) => head)
    }

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

    override def filter(
      conditionExprBuilder: A =~:> Boolean,
    )(implicit
      opO: OP[C[A]],
      opA: OP[A],
      opB: OP[Boolean],
      filterC: FunctorFilter[C],
    ): AndThen[I, C[A], C[A]] =
      inputExpr.andThen(Expr.Filter(conditionExprBuilder(ident)))

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

    override def foldLeft[CI <: I, B](
      initExpr: CI ~:> B,
    )(
      foldExprBuilder: ((B, A) ~:> B, (B, A) ~:> A) => ((B, A) ~:> B),
    )(implicit
      foldableC: Foldable[C],
      opBA: OP[(B, A)],
      opA: OP[A],
      opB: OP[B],
    ): Expr.FoldLeft[CI, C, A, B, OP] = {
      val initLensExpr = ident[(B, A)]
      val b = initLensExpr.get(_.at(Nat._0))
      val a = initLensExpr.get(_.at(Nat._1))
      val foldExpr = foldExprBuilder(b, a)
      Expr.FoldLeft(inputExpr, initExpr, foldExpr)
    }

    override def map[B](
      mapExprBuilder: A =~:> B,
    )(implicit
      opI: OP[A],
      opA: OP[C[A]],
      opB: OP[C[B]],
      functorC: Functor[C],
    ): AndThen[I, C[A], C[B]] =
      inputExpr.andThen(Expr.MapEvery[C, A, B, OP](mapExprBuilder(ident)))

    override def flatMap[D[a] >: C[a] : FlatMap, O](
      exprBuilder: A =~:> D[O],
    )(implicit
      opA: OP[A],
      opDDO: OP[D[D[O]]],
      opDO: OP[D[O]],
    ): AndThen[I, D[D[O]], D[O]] =
      inputExpr.andThen(Expr.MapEvery[D, A, D[O], OP](exprBuilder(ident))).andThen(Expr.Flatten())

    override def isEmpty(
      implicit
      sizeCompare: SizeComparable[C[A], Int, Boolean],
      opI: OP[Int],
      opWI: OP[Int],
      opWB: OP[Boolean],
    ): AndThen[I, C[A], Boolean] =
      inputExpr.andThen(Expr.SizeIs[C[A], Int, Boolean, OP](SizeComparison.===, Expr.Const(0)(opWI)))

    override def sizeIs: UnwrappedSizeIsBuilder[I, C[A]] = new UnwrappedSizeIsBuilder(inputExpr)

    override def sorted(
      implicit
      sortable: Sortable[C, A],
      opAs: OP[C[A]],
    ): AndThen[I, C[A], C[A]] =
      inputExpr.andThen(Expr.Sorted())
  }

  final class UnwrappedSizeIsBuilder[-I, C](inputExpr: I ~:> C) extends SizeIsBuilder(inputExpr) {

    override def ===(
      sizeExpr: C ~:> Int,
    )(implicit
      sizeComparable: SizeComparable[C, Int, Boolean],
      opO: OP[Boolean],
    ): AndThen[I, C, Boolean] =
      inputExpr.andThen {
        Expr.SizeIs(SizeComparison.===, sizeExpr)
      }

    override def >(
      sizeExpr: C ~:> Int,
    )(implicit
      sizeComparable: SizeComparable[C, Int, Boolean],
      opO: OP[Boolean],
    ): AndThen[I, C, Boolean] =
      inputExpr.andThen {
        Expr.SizeIs(SizeComparison.>, sizeExpr)
      }

    override def >=(
      sizeExpr: C ~:> Int,
    )(implicit
      sizeComparable: SizeComparable[C, Int, Boolean],
      opO: OP[Boolean],
    ): AndThen[I, C, Boolean] =
      inputExpr.andThen {
        Expr.SizeIs(SizeComparison.>=, sizeExpr)
      }

    override def <(
      sizeExpr: C ~:> Int,
    )(implicit
      sizeComparable: SizeComparable[C, Int, Boolean],
      opO: OP[Boolean],
    ): AndThen[I, C, Boolean] =
      inputExpr.andThen {
        Expr.SizeIs(SizeComparison.<, sizeExpr)
      }

    override def <=(
      sizeExpr: C ~:> Int,
    )(implicit
      sizeComparable: SizeComparable[C, Int, Boolean],
      opO: OP[Boolean],
    ): AndThen[I, C, Boolean] =
      inputExpr.andThen {
        Expr.SizeIs(SizeComparison.<=, sizeExpr)
      }
  }
}
