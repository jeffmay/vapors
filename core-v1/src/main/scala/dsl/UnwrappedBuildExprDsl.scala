package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data.{Extract, FactType, FactTypeSet, SliceRange, TypedFact}
import lens.{CollectInto, IterableInto, VariantLens}
import logic.Logic
import math.{Add, Power}

import cats.data.NonEmptySeq
import cats.{FlatMap, Foldable, Functor, Id, Order, Reducible, Traverse}
import shapeless3.deriving.K0

trait UnwrappedBuildExprDsl
  extends BuildExprDsl
  with UnwrappedUsingDefinitionArityMethods
  with DefaultUnwrappedExprHListImplicits
  with DefaultUnwrappedOutputTypeImplicits
  with UnwrappedTimeFunctions
  with UnwrappedDslTypes {

  override protected implicit final def boolLogic: Logic[W, Boolean, OP] = Logic.bool

  override protected implicit final def windowComparable: WindowComparable[W, OP] = WindowComparable.identity

  override protected implicit final def extract: Extract[W] = Extract.identity

  override protected implicit final def wrapConst: WrapConst[W, OP] = WrapConst.unwrapped

  override protected implicit final def wrapContained: WrapContained[W, OP] = WrapContained.unwrapped

  override protected implicit final def wrapSelected: WrapSelected[W, OP] = WrapSelected.unwrapped

  // TODO: Should this be visible outside this trait?
  protected final def shortCircuit: Boolean = true

  override final def ident[I](implicit opI: OP[I]): Expr.Identity[I, OP] = Expr.Identity()

  override final def seq[I, O](expressions: I ~:> O*)(implicit opO: OP[Seq[O]]): I ~:> Seq[O] =
    super.seq[I, O](expressions: _*)

  override final def some[I, O](expr: I ~:> O)(implicit opO: OP[Option[O]]): I ~:> Option[O] = super.some[I, O](expr)

  override final def none[O](implicit opO: OP[Option[O]]): Any ~:> Option[O] = super.none[O]

  override def define[T](factType: FactType[T]): UnwrappedDefineBuilder[T] = new UnwrappedDefineBuilder(factType)

  final class UnwrappedDefineBuilder[T](factType: FactType[T]) extends DefineBuilder(factType) {

    override def oneFrom(
      defnExpr: Any ~:> T,
    )(implicit
      opWT: OP[T],
      opT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[Any, Id, T, OP] =
      Expr.Define(factType, defnExpr: Any ~:> Id[T])

    override def from[C[_] : Functor : Foldable](
      defnExpr: Any ~:> C[T],
    )(implicit
      opWT: OP[T],
      opCT: OP[C[T]],
      opT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[Any, C, T, OP] =
      Expr.Define(factType, defnExpr)

    override def fromInput[I, C[_] : Functor : Foldable](
      buildDefnExpr: I =~:> C[T],
    )(implicit
      opI: OP[I],
      opWT: OP[T],
      opCT: OP[C[T]],
      opT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[I, C, T, OP] =
      Expr.Define(factType, buildDefnExpr(Expr.Identity()))

  }

  override final def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opT: OP[T],
    opTs: OP[Seq[T]],
  ): Expr.ValuesOfType[T, T, OP] =
    Expr.ValuesOfType(factTypeSet, _.value)

  override final def min[I, N : Order](
    first: I ~:> N,
    rest: I ~:> N*,
  )(implicit
    opSWN: OP[NonEmptySeq[N]],
    opWN: OP[N],
  ): I ~:> N =
    super.min[I, N](first, rest: _*)

  override final def max[I, N : Order](
    first: I ~:> N,
    rest: I ~:> N*,
  )(implicit
    opSWN: OP[NonEmptySeq[N]],
    opWN: OP[N],
  ): I ~:> N =
    super.max[I, N](first, rest: _*)

  override final def sum[I, N : Numeric](
    first: I ~:> N,
    rest: I ~:> N*,
  )(implicit
    addWN: Add.Id[N],
    opSWN: OP[NonEmptySeq[N]],
    opN: OP[N],
    opTWN: OP[(N, N)],
    opWN: OP[N],
  ): I ~:> N = super.sum[I, N](first, rest: _*)(Numeric[N], addWN, opSWN, opN, opTWN, opWN)

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
    override def thenReturn[TI <: I, TO >: O](thenExpr: TI ~:> TO): UnwrappedWhenElseBuilder[TI, TO] =
      new UnwrappedWhenElseBuilder(branches :+ Expr.ConditionBranch(nextCondExpr, thenExpr))
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

  // TODO: Is this redundant syntax worth keeping around?
  override implicit final def inSet[I, A](inputExpr: I ~:> A): UnwrappedInSetExprBuilder[I, A] =
    new UnwrappedInSetExprBuilder(inputExpr)

  final class UnwrappedInSetExprBuilder[-I, +A](inputExpr: I ~:> A) extends InSetExprBuilder(inputExpr) {

    override def in[NI <: I, V >: A](
      validValuesExpr: NI ~:> Set[V],
    )(implicit
      opA: OP[V],
      opO: OP[Boolean],
    ): Expr.ContainsAny[NI, W, Id, V, Boolean, OP] =
      Expr.ContainsAny[NI, W, Id, V, Boolean, OP](inputExpr, validValuesExpr, wrapContained.wrapContained(_, _, _))
  }

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

  override implicit final def xhlOps[I, WL <: Tuple](xhl: ExprHList[I, WL, OP]): UnwrappedExprHListOpsBuilder[I, WL] =
    new UnwrappedExprHListOpsBuilder(xhl)

  final class UnwrappedExprHListOpsBuilder[-I, WL <: Tuple](inputExprHList: ExprHList[I, WL, OP])
    extends ExprHListOpsBuilder(inputExprHList) {

    override def toHList[UL <: Tuple](
      implicit
      zip: ZipToShortest.Aux[W, WL, OP, UL],
      opO: OP[UL],
    ): I ~:> UL = {
      val expr = Expr.ZipToShortestHList(inputExprHList)
      expr: I ~:> UL // let the compiler prove that W[UL] == UL after computing the correct mapN implicit above
    }

    // IntelliJ highlights this as an error, but it compiles and it provides better type inference for the return type
    override def zipToShortest[C[+_], UL <: Tuple](
      implicit
      zip: ZipToShortest.Aux[C, WL, OP, UL],
      opO: OP[C[UL]],
    ): I ~:> C[UL] = {
      val expr = Expr.ZipToShortestHList[I, C, WL, UL, OP](inputExprHList)(zip, opO)
      expr: I ~:> C[UL] // let the compiler prove that C[W[UL]] == C[UL] after computing the correct mapN implicit above
    }
  }

  override implicit def fromHL[I, L <: Tuple](expr: I ~:> L): UnwrappedConvertHListExprBuilder[I, L] =
    new UnwrappedConvertHListExprBuilder(expr)

  final class UnwrappedConvertHListExprBuilder[-I, L <: Tuple](inputExpr: I ~:> L)
    extends ConvertHListExprBuilder(inputExpr) {

    override def as[P](
      implicit
      gen: K0.Generic[P],
      opL: OP[L],
      opWL: OP[L],
      opP: OP[P],
      opWP: OP[P],
    ): AndThen[I, L, P] = ???
//      inputExpr.andThen(Expr.Convert(ExprConverter.asProductType)(opWP))(opWP)
  }

  override implicit def optionOps[I, O](optExpr: I ~:> Option[O]): UnwrappedOptionExprBuilder[I, O] =
    new UnwrappedOptionExprBuilder(optExpr)

  final class UnwrappedOptionExprBuilder[-I, +O](optExpr: I ~:> Option[O]) extends OptionExprBuilder(optExpr) {

    override def getOrElse[EI <: I, EO >: O](defaultExpr: EI ~:> EO)(implicit opO: OP[EO]): Expr.GetOrElse[EI, EO, OP] =
      Expr.GetOrElse(optExpr, defaultExpr)
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

  final class UnwrappedHkExprBuilder[-I, C[_], A](inputExpr: I ~:> C[A]) extends HkExprBuilder(inputExpr) {

    override def atIndex(
      index: Long,
    )(implicit
      foldableC: Foldable[C],
      opA: OP[A],
      opO: OP[Option[A]],
    ): Expr.Select[I, C[A], Option[A], Option[A], OP] = {
      val lens = VariantLens.id[C[A]].at(index)
      Expr.Select(inputExpr, lens, (_, el) => el)
    }

    override def containsAny[NI <: I](
      validValuesExpr: NI ~:> Set[A],
    )(implicit
      foldableC: Foldable[C],
      opA: OP[A],
      opO: OP[Boolean],
    ): Expr.ContainsAny[NI, W, C, A, Boolean, OP] =
      Expr.ContainsAny[NI, W, C, A, Boolean, OP](inputExpr, validValuesExpr, wrapContained.wrapContained(_, _, _))

    override def head(
      implicit
      reducibleC: Reducible[C],
      opA: OP[A],
    ): Expr.Select[I, C[A], A, A, OP] = {
      val lens = VariantLens.id[C[A]].head[C, A]
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

    override def filter[D[_]](
      conditionExprBuilder: A =~:> Boolean,
    )(implicit
      filter: CollectInto.Filter[C, A, D],
      opA: OP[A],
      opO: OP[D[A]],
    ): AndThen[I, C[A], D[A]] =
      inputExpr.andThen(Expr.Filter[C, A, Boolean, D, OP](conditionExprBuilder(ident)))

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
//      val b = initLensExpr.get(_.at(0))
//      val a = initLensExpr.get(_.at(1))
//      val foldExpr = foldExprBuilder(b, a)
//      Expr.FoldLeft(inputExpr, initExpr, foldExpr)
      ???
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

    override def min(
      implicit
      reducibleC: Reducible[C],
      orderA: Order[A],
      opO: OP[A],
    ): AndThen[I, C[A], A] =
      inputExpr.andThen {
        Expr.CustomFunction("min", Reducible[C].minimum(_: C[A]))
      }

    override def max(
      implicit
      reducibleC: Reducible[C],
      orderA: Order[A],
      opO: OP[A],
    ): AndThen[I, C[A], A] =
      inputExpr.andThen {
        Expr.CustomFunction("min", Reducible[C].maximum(_: C[A]))
      }

    override def isEmpty(
      implicit
      sizeCompare: SizeComparable[C[A], Int, Boolean],
      opI: OP[Int],
      opWI: OP[Int],
      opWB: OP[Boolean],
    ): AndThen[I, C[A], Boolean] =
      inputExpr.andThen(Expr.SizeIs[C[A], Int, Boolean, OP](SizeComparison.===, Expr.Const(0)(opWI)))

    override def sizeIs: UnwrappedSizeIsBuilder[I, C[A]] = new UnwrappedSizeIsBuilder(inputExpr)

    override def slice[D[_]](
      range: SliceRange.Relative,
    )(implicit
      traverseC: Traverse[C],
      filter: CollectInto.Filter[C, A, D],
      opO: OP[D[A]],
    ): AndThen[I, C[A], D[A]] =
      inputExpr.andThen(Expr.Slice(range))

    override def sorted(
      implicit
      sortable: Sortable[C, A],
      opAs: OP[C[A]],
    ): AndThen[I, C[A], C[A]] =
      inputExpr.andThen(Expr.Sorted())

    override def sum(
      implicit
      foldableC: Foldable[C],
      addA: Add.Id[A],
      numericA: Numeric[A],
      opA: OP[A],
      opAA: OP[(A, A)],
      opO: OP[A],
    ): Expr.FoldLeft[I, C, A, A, OP] = super.sum(foldableC, addA, numericA, opA, opAA, opO)

    override def to[S[_]](
      implicit
      foldableC: Foldable[C],
      into: IterableInto[S, A],
    ): SelectHolder[I, C[A], into.Out, into.Out, OP] = {
      val lens = VariantLens.id[C[A]].to(into)
      new SelectHolder(inputExpr, lens, (_, out) => out)
    }
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
