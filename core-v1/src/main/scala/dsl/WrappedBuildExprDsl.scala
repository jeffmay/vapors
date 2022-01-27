package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data._
import lens.{CollectInto, IterableInto, VariantLens}
import math.Power

import cats.data.NonEmptySeq
import cats.{FlatMap, Foldable, Functor, Id, Order, Reducible, Traverse}

trait WrappedBuildExprDsl extends BuildExprDsl {
  self: DslTypes with WrappedExprHListDslImplicits with OutputTypeImplicits =>

  protected implicit def extract: Extract[W]

  protected implicit def extractBool[B : ExtractValue.AsBoolean]: ExtractValue.AsBoolean[W[B]]

  protected implicit def wrapQuantifier: WrapQuantifier[W, OP]

  protected implicit def wrapFact: WrapFact[W, OP]

  protected implicit def wrapSelected: WrapSelected[W, OP]

  override def ident[I](implicit opI: OP[W[I]]): Expr.Identity[W[I], OP] = Expr.Identity()

  override def pow[I, L, R](
    leftExpr: I ~:> W[L],
    rightExpr: I ~:> W[R],
  )(implicit
    opR: OP[W[R]],
    pow: Power[W[L], W[R]],
  ): CombineHolder[I, W[L], W[L], W[R], W[R], pow.Out, OP] =
    (leftExpr ^ rightExpr)(opR, pow)

  override def when[I](condExpr: I ~:> W[Boolean]): WrappedWhenBuilder[I] = new WrappedWhenBuilder(condExpr)

  class WrappedWhenBuilder[-I](firstCondExpr: I ~:> W[Boolean]) extends WhenBuilder[I, W[Boolean]](firstCondExpr) {
    override def thenReturn[TI <: I, TO](thenExpr: TI ~:> TO): WrappedWhenElseBuilder[TI, TO] =
      new WrappedWhenElseBuilder(NonEmptySeq.of(Expr.ConditionBranch(firstCondExpr, thenExpr)))
  }

  class WrappedWhenElifBuilder[-I, +O](
    branches: NonEmptySeq[Expr.ConditionBranch[I, W[Boolean], O, OP]],
    nextCondExpr: I ~:> W[Boolean],
  ) extends WhenElifBuilder(branches, nextCondExpr) {

    override def thenReturn[TI <: I, TO >: O](thenExpr: TI ~:> TO): WrappedWhenElseBuilder[TI, TO] =
      new WrappedWhenElseBuilder(branches :+ Expr.ConditionBranch(nextCondExpr, thenExpr))
  }

  class WrappedWhenElseBuilder[-I, +O](branches: NonEmptySeq[Expr.ConditionBranch[I, W[Boolean], O, OP]])
    extends WhenElseBuilder(branches) {

    override def elif[CI <: I](nextCondExpr: CI ~:> W[Boolean]): WrappedWhenElifBuilder[CI, O] =
      new WrappedWhenElifBuilder(branches, nextCondExpr)

    override def elseReturn[EI <: I, EO >: O](
      elseExpr: EI ~:> EO,
    )(implicit
      opO: OP[EO],
    ): Expr.When[EI, W[Boolean], EO, OP] =
      Expr.When(branches, elseExpr)
  }

  override def define[T](factType: FactType[T]): WrappedDefineBuilder[T] = new WrappedDefineBuilder(factType)

  class WrappedDefineBuilder[T](factType: FactType[T]) extends DefineBuilder(factType) {

    override def oneFrom(
      defnExpr: Any ~:> W[T],
    )(implicit
      opWT: OP[W[T]],
      opT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[Any, Id, T, OP] = {
      val lens = VariantLens.id[W[T]].extractValue[W, T]
      val extracted = defnExpr.andThen {
        Expr.Select(ident, lens, (_: W[T], t: T) => t)
      }
      Expr.Define(factType, extracted: Any ~:> Id[T])
    }

    override def from[C[_] : Functor : Foldable](
      defnExpr: Any ~:> C[W[T]],
    )(implicit
      opWT: OP[W[T]],
      opCT: OP[C[T]],
      opT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[Any, C, T, OP] = {
      val lens = VariantLens.id[W[T]].extractValue[W, T]
      val extracted = defnExpr.andThen {
        Expr.MapEvery[C, W[T], T, OP](Expr.Select(ident, lens, (_: W[T], t: T) => t))
      }
      Expr.Define(factType, extracted: Any ~:> C[T])
    }

    override def fromInput[I, C[_] : Functor : Foldable](
      buildDefnExpr: I =~:> C[W[T]],
    )(implicit
      opI: OP[I],
      opWT: OP[W[T]],
      opCT: OP[C[T]],
      opT: OP[T],
      opF: OP[Seq[TypedFact[T]]],
    ): Expr.Define[I, C, T, OP] = {
      val unextracted = buildDefnExpr(Expr.Identity())
      val lens = VariantLens.id[W[T]].extractValue[W, T]
      val extracted = unextracted.andThen {
        Expr.MapEvery[C, W[T], T, OP](Expr.Select(ident, lens, (_: W[T], t: T) => t))
      }
      Expr.Define(factType, extracted)
    }
  }

  override def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opT: OP[T],
    opTs: OP[Seq[W[T]]],
  ): Expr.ValuesOfType[T, W[T], OP] =
    Expr.ValuesOfType(factTypeSet, wrapFact.wrapFact(_))

  override implicit def inSet[I, A](inputExpr: I ~:> W[A]): WrappedInSetExprBuilder[I, A] =
    new WrappedInSetExprBuilder(inputExpr)

  class WrappedInSetExprBuilder[-I, +A](inputExpr: I ~:> W[A]) extends InSetExprBuilder(inputExpr) {

    override def in[NI <: I, V >: A](
      validValuesExpr: NI ~:> Set[W[V]],
    )(implicit
      opA: OP[V],
      opO: OP[W[Boolean]],
    ): Expr.ContainsAny[NI, W, Id, V, W[Boolean], OP] =
      Expr.ContainsAny[NI, W, Id, V, W[Boolean], OP](
        inputExpr: I ~:> W[V], // prove to IntelliJ that inputExpr has the correct type via covariance
        validValuesExpr,
        wrapContained.wrapContained(_, _, _),
      )
  }

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

  override implicit def xhlOps[I, WL <: Tuple](exprHList: ExprHList[I, WL, OP]): WrappedExprHListOpsBuilder[I, WL] =
    new WrappedExprHListOpsBuilder(exprHList)

  class WrappedExprHListOpsBuilder[-I, WL <: Tuple](inputExprHList: ExprHList[I, WL, OP])
    extends ExprHListOpsBuilder(inputExprHList) {

    override def toHList[UL <: Tuple](
      implicit
      isCons: ZipToShortest.Aux[W, WL, OP, UL],
      opO: OP[W[UL]],
    ): I ~:> W[UL] =
      Expr.ZipToShortestHList(inputExprHList)

    override def zipToShortest[C[+_], UL <: Tuple](
      implicit
      zip: [a] =>> ZipToShortest.Aux[C[W[a]], WL, OP, UL],
      opO: OP[C[W[UL]]],
    ): I ~:> C[W[UL]] =
      Expr.ZipToShortestHList[I, [a] =>> C[W[a]], WL, UL, OP](inputExprHList)(zip, opO)
  }

  override implicit def fromHL[I, L <: Tuple](expr: I ~:> W[L]): WrappedConvertHListExprBuilder[I, L] =
    new WrappedConvertHListExprBuilder(expr)

  class WrappedConvertHListExprBuilder[-I, L <: Tuple](inputExpr: I ~:> W[L])
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

  override implicit def sizeOf[I, C](inputExpr: I ~:> C): WrappedSizeOfExprBuilder[I, C] =
    new WrappedSizeOfExprBuilder(inputExpr)

  class WrappedSizeOfExprBuilder[-I, C](inputExpr: I ~:> C) extends SizeOfExprBuilder(inputExpr) {

    override def isEmpty(
      implicit
      sizeCompare: SizeComparable[C, W[Int], W[Boolean]],
      opI: OP[Int],
      opWI: OP[W[Int]],
      opWB: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]] =
      inputExpr.andThen {
        Expr.SizeIs[C, W[Int], W[Boolean], OP](SizeComparison.===, Expr.Const(wrapConst.wrapConst(0)))
      }

    override def sizeIs: SizeIsBuilder[I, C] = new WrappedSizeIsBuilder(inputExpr)
  }

  override implicit def hk[I, C[_], A](
    expr: I ~:> C[W[A]],
  )(implicit
    ne: NotEmpty[C, A],
  ): WrappedHkExprBuilder[I, C, A] = new WrappedHkExprBuilder(expr)

  class WrappedHkExprBuilder[-I, C[_], A](inputExpr: I ~:> C[W[A]]) extends HkExprBuilder(inputExpr) {

    override def atIndex(
      index: Long,
    )(implicit
      foldableC: Foldable[C],
      opA: OP[A],
      opO: OP[Option[W[A]]],
    ): Expr.Select[I, C[W[A]], Option[W[A]], Option[W[A]], OP] = {
      val lens = VariantLens.id[C[W[A]]].at(index)
      Expr.Select(inputExpr, lens, (_, el) => el)
    }

    override def containsAny[NI <: I](
      validValuesExpr: NI ~:> Set[W[A]],
    )(implicit
      traverseC: Foldable[C],
      opA: OP[A],
      opO: OP[W[Boolean]],
    ): Expr.ContainsAny[NI, W, C, A, W[Boolean], OP] = {
      Expr.ContainsAny(
        inputExpr,
        validValuesExpr,
        wrapContained.wrapContained(_, _, _),
      )
    }

    override def head(
      implicit
      reducibleC: Reducible[C],
      opA: OP[W[A]],
    ): Expr.Select[I, C[W[A]], W[A], W[A], OP] = {
      val lens = VariantLens.id[C[W[A]]].head[C, W[A]]
      Expr.Select(inputExpr, lens, (_, head) => head)
    }

    override def headOption(
      implicit
      foldableC: Foldable[C],
      opA: OP[A],
      opO: OP[Option[W[A]]],
    ): Expr.Select[I, C[W[A]], Option[W[A]], Option[W[A]], OP] = {
      val lens = VariantLens.id[C[W[A]]].at(0)
      Expr.Select(inputExpr, lens, (_, opt) => opt)
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

    override def foldLeft[CI <: I, B](
      initExpr: CI ~:> W[B],
    )(
      foldExprBuilder: ((W[B], W[A]) ~:> W[B], (W[B], W[A]) ~:> W[A]) => ((W[B], W[A]) ~:> W[B]),
    )(implicit
      foldableC: Foldable[C],
      opBA: OP[(W[B], W[A])],
      opA: OP[W[A]],
      opB: OP[W[B]],
    ): Expr.FoldLeft[CI, C, W[A], W[B], OP] = {
      val initLensExpr = Expr.Identity[(W[B], W[A]), OP]()
      val initLens = VariantLens.id[(W[B], W[A])]
      val b = Expr.Select(initLensExpr, initLens.at(0), (_: (W[B], W[A]), wb: W[B]) => wb)
      val a = Expr.Select(initLensExpr, initLens.at(1), (_: (W[B], W[A]), wa: W[A]) => wa)
      val foldExpr = foldExprBuilder(b, a)
      Expr.FoldLeft(inputExpr, initExpr, foldExpr)
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

    override def filter[D[_]](
      conditionExprBuilder: W[A] =~:> W[Boolean],
    )(implicit
      filter: CollectInto.Filter[C, W[A], D],
      opA: OP[W[A]],
      opO: OP[D[W[A]]],
    ): AndThen[I, C[W[A]], D[W[A]]] =
      inputExpr.andThen(Expr.Filter[C, W[A], W[Boolean], D, OP](conditionExprBuilder(Expr.Identity())))

    override def flatMap[D[a] >: C[a] : FlatMap, O](
      exprBuilder: W[A] =~:> D[W[O]],
    )(implicit
      opA: OP[W[A]],
      opDDO: OP[D[D[W[O]]]],
      opDO: OP[D[W[O]]],
    ): AndThen[I, D[D[W[O]]], D[W[O]]] =
      inputExpr.andThen(Expr.MapEvery[D, W[A], D[W[O]], OP](exprBuilder(ident))).andThen(Expr.Flatten())

    override def min(
      implicit
      reducibleC: Reducible[C],
      orderA: Order[A],
      opO: OP[W[A]],
    ): AndThen[I, C[W[A]], W[A]] =
      inputExpr.andThen {
        Expr.CustomFunction("min", Reducible[C].minimumBy(_)(extract.extract))
      }

    override def max(
      implicit
      reducibleC: Reducible[C],
      orderA: Order[A],
      opO: OP[W[A]],
    ): AndThen[I, C[W[A]], W[A]] =
      inputExpr.andThen {
        Expr.CustomFunction("max", Reducible[C].maximumBy(_)(extract.extract))
      }

    override def isEmpty(
      implicit
      sizeCompare: SizeComparable[C[W[A]], W[Int], W[Boolean]],
      opI: OP[Int],
      opWI: OP[W[Int]],
      opWB: OP[W[Boolean]],
    ): AndThen[I, C[W[A]], W[Boolean]] =
      inputExpr.andThen {
        Expr.SizeIs[C[W[A]], W[Int], W[Boolean], OP](SizeComparison.===, Expr.Const(wrapConst.wrapConst(0)))
      }

    override def sizeIs: SizeIsBuilder[I, C[W[A]]] = new WrappedSizeIsBuilder(inputExpr)

    override def slice[D[_]](
      range: SliceRange.Relative,
    )(implicit
      traverseC: Traverse[C],
      filter: CollectInto.Filter[C, W[A], D],
      opO: OP[D[W[A]]],
    ): AndThen[I, C[W[A]], D[W[A]]] =
      inputExpr.andThen(Expr.Slice(range))

    override def sorted(
      implicit
      sortable: Sortable[C, W[A]],
      opAs: OP[C[W[A]]],
    ): AndThen[I, C[W[A]], C[W[A]]] =
      inputExpr.andThen(Expr.Sorted())

    override def to[S[_]](
      implicit
      foldableC: Foldable[C],
      into: IterableInto[S, W[A]],
    ): SelectHolder[I, C[W[A]], into.Out, into.Out, OP] = {
      val lens = VariantLens.id[C[W[A]]].to(into)
      // This is an isomorphic operation at the container level, so there is no need to alter the wrapped elements
      new SelectHolder(inputExpr, lens, (_, out) => out)
    }
  }

  class WrappedSizeIsBuilder[-I, C](inputExpr: I ~:> C) extends SizeIsBuilder(inputExpr) {

    override def ===(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]] =
      inputExpr.andThen {
        Expr.SizeIs[C, W[Int], W[Boolean], OP](SizeComparison.===, sizeExpr)
      }

    override def >(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]] =
      inputExpr.andThen {
        Expr.SizeIs[C, W[Int], W[Boolean], OP](SizeComparison.>, sizeExpr)
      }

    override def >=(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]] =
      inputExpr.andThen {
        Expr.SizeIs[C, W[Int], W[Boolean], OP](SizeComparison.>=, sizeExpr)
      }

    override def <(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]] =
      inputExpr.andThen {
        Expr.SizeIs[C, W[Int], W[Boolean], OP](SizeComparison.<, sizeExpr)
      }

    override def <=(
      sizeExpr: C ~:> W[Int],
    )(implicit
      sizeComparable: SizeComparable[C, W[Int], W[Boolean]],
      opO: OP[W[Boolean]],
    ): AndThen[I, C, W[Boolean]] =
      inputExpr.andThen {
        Expr.SizeIs[C, W[Int], W[Boolean], OP](SizeComparison.<=, sizeExpr)
      }
  }
}
