package com.rallyhealth.vapors.v1

package dsl

import algebra.{CombineHolder, Expr, ExprConverter}
import data.{Extract, ExtractValue, FactTypeSet}
import lens.VariantLens
import math.Power

import cats.{Foldable, Functor, FunctorFilter}
import shapeless.{Generic, HList}

trait WrappedExprDsl extends BuildExprDsl {
  self: DslTypes with WrapImplicits =>

  protected implicit def extract: Extract[W]

  protected implicit def extractBool[B : ExtractValue.AsBoolean]: ExtractValue.AsBoolean[W[B]]

  protected implicit def wrapQuantifier: WrapQuantifier[W, OP]

  protected implicit def wrapFact: WrapFact[W, OP]

  protected implicit def wrapSelected: WrapSelected[W, OP]

  override def ident[I](implicit opI: OP[W[I]]): Expr.Identity[W[I], OP] = Expr.Identity()

  override def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opT: OP[T],
    opTs: OP[Seq[W[T]]],
  ): Expr.ValuesOfType[T, W[T], OP] =
    Expr.ValuesOfType(factTypeSet, wrapFact.wrapFact(_))

  override def pow[I, L, R](
    leftExpr: I ~:> W[L],
    rightExpr: I ~:> W[R],
  )(implicit
    opR: OP[W[R]],
    pow: Power[W[L], W[R]],
  ): CombineHolder[I, W[L], W[L], W[R], W[R], pow.Out, OP] =
    (leftExpr ^ rightExpr)(opR, pow)

  override implicit def const[A](
    value: A,
  )(implicit
    constType: ConstOutputType[W, A],
  ): ConstExprBuilder[constType.Out, OP] =
    new ConstExprBuilder(constType.wrapConst(value))

  override implicit def in[I, T](expr: I ~:> W[T]): WrappedSelectExprBuilder[I, T] = new WrappedSelectExprBuilder(expr)

  class WrappedSelectExprBuilder[-I, A](inputExpr: I ~:> W[A]) extends SelectExprBuilder(inputExpr) {

    override def get[B : Wrappable, O](
      selector: VariantLens.FromTo[A, B],
    )(implicit
      sot: SelectOutputType.Aux[W, A, B, O],
      opO: OP[O],
    ): Expr.Select[I, W[A], B, O, OP] = {
      val lens = VariantLens.id[W[A]].extractValue.andThen(selector(VariantLens.id[A]))
      Expr.Select(inputExpr, lens, sot.wrapSelected(_, lens.path, _))
    }

    override def getAs[C[_]]: GetAsWrapper[I, W, A, C, OP] = new GetAsWrapper(inputExpr)
  }

  override implicit def xhlOps[I, WL <: HList](exprHList: ExprHList[I, WL, OP]): WrappedExprHListOpsBuilder[I, WL] =
    new WrappedExprHListOpsBuilder(exprHList)

  class WrappedExprHListOpsBuilder[-I, WL <: HList](inputExprHList: ExprHList[I, WL, OP])
    extends ExprHListOpsBuilder(inputExprHList) {

    override def toHList[UL <: HList](
      implicit
      isCons: ZipToShortest.Aux[W, WL, OP, UL],
      opO: OP[W[UL]],
    ): I ~:> W[UL] =
      Expr.ZipToShortestHList(inputExprHList)

    override def zipToShortest[C[+_], UL <: HList](
      implicit
      zip: ZipToShortest.Aux[CW[C, W, +*], WL, OP, UL],
      opO: OP[C[W[UL]]],
    ): I ~:> C[W[UL]] =
      Expr.ZipToShortestHList[I, CW[C, W, +*], WL, UL, OP](inputExprHList)(zip, opO)
  }

  override implicit def fromHL[I, L <: HList](expr: I ~:> W[L]): WrappedConvertHListExprBuilder[I, L] =
    new WrappedConvertHListExprBuilder(expr)

  class WrappedConvertHListExprBuilder[-I, L <: HList](inputExpr: I ~:> W[L])
    extends ConvertHListExprBuilder(inputExpr) {

    override def as[P](
      implicit
      gen: Generic.Aux[P, L],
      opL: OP[L],
      opWL: OP[W[L]],
      opP: OP[P],
      opWP: OP[W[P]],
    ): AndThen[I, W[L], W[P]] =
      inputExpr.andThen(Expr.Convert(ExprConverter.asWrappedProductType[W, L, P, OP]))
  }

  override implicit def hk[I, C[_], A](
    expr: I ~:> C[W[A]],
  )(implicit
    ne: NotEmpty[C, A],
  ): WrappedHkExprBuilder[I, C, A] = new WrappedHkExprBuilder(expr)

  override type SpecificHkExprBuilder[-I, C[_], A] = WrappedHkExprBuilder[I, C, A]

  class WrappedHkExprBuilder[-I, C[_], A](inputExpr: I ~:> C[W[A]]) extends HkExprBuilder(inputExpr) {

    override def headOption(
      implicit
      foldableC: Foldable[C],
      opA: OP[A],
      opO: OP[Option[W[A]]],
    ): Expr.Select[I, C[W[A]], Option[W[A]], Option[W[A]], OP] = {
      val lens = VariantLens.id[C[W[A]]].at(0)
      Expr.Select(inputExpr, lens, (cwa, opt) => opt)
    }

    override def exists(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      foldC: Foldable[C],
    ): AndThen[I, C[W[A]], W[Boolean]] = {
      val condExpr = conditionExprBuilder(Expr.Identity())
      inputExpr.andThen(
        Expr.Exists[C, W[A], W[Boolean], OP](
          condExpr,
          wrapQuantifier.wrapTrueExists(_),
          wrapQuantifier.wrapFalseExists(_),
          wrapQuantifier.shortCircuit,
        ),
      )
    }

    override def forall(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      foldC: Foldable[C],
    ): AndThen[I, C[W[A]], W[Boolean]] = {
      val condExpr = conditionExprBuilder(Expr.Identity())
      inputExpr.andThen(
        Expr.ForAll[C, W[A], W[Boolean], OP](
          condExpr,
          wrapQuantifier.wrapTrueForAll(_),
          wrapQuantifier.wrapFalseForAll(_),
          wrapQuantifier.shortCircuit,
        ),
      )
    }

    override def map[B](
      mapExprBuilder: W[A] =~:> W[B],
    )(implicit
      opI: OP[W[A]],
      opA: OP[C[W[A]]],
      opB: OP[C[W[B]]],
      functorC: Functor[C],
    ): AndThen[I, C[W[A]], C[W[B]]] =
      inputExpr.andThen(Expr.MapEvery(mapExprBuilder(ident)))

    override def filter(
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      opO: OP[C[W[A]]],
      opA: OP[W[A]],
      opB: OP[W[Boolean]],
      filterC: FunctorFilter[C],
    ): AndThen[I, C[W[A]], C[W[A]]] =
      inputExpr.andThen(Expr.Filter(conditionExprBuilder(Expr.Identity())))

    override def sorted(
      implicit
      sortable: Sortable[C, W[A]],
      opAs: OP[C[W[A]]],
    ): AndThen[I, C[W[A]], C[W[A]]] =
      inputExpr.andThen(Expr.Sorted())
  }
}
