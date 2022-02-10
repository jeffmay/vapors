package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data.{Extract, ExtractValue, Justified}
import logic.Logic

import cats.{Align, Functor, FunctorFilter, Traverse}
import shapeless.{::, <:!<, HList, HNil}

import scala.collection.Factory

trait JustifiedBuildExprDsl
  extends WrappedBuildExprDsl
  with DefinedJustifiedDslImplicitDefinitions
  with JustifiedExprHListDslImplicits
  with JustifiedOutputTypeImplicits
  with JustifiedDslTypes {

  override protected implicit final def boolLogic: Logic[Justified, Boolean, OP] = Justified.bool[OP]

  override protected implicit final def windowComparable: WindowComparable[Justified, OP] = WindowComparable.justified

  override protected implicit final def extract: Extract[Justified] = Justified.extract

  override protected implicit final def extractBool[
    B : ExtractValue.AsBoolean,
  ]: ExtractValue.AsBoolean[Justified[B]] = { b =>
    ExtractValue.asBoolean(b.value)
  }

  override protected implicit final def wrapConst: WrapConst[Justified, OP] = Justified.wrapConst

  override protected implicit final def wrapContained: WrapContained[Justified, OP] = Justified.wrapContained

  override protected implicit final def wrapFact: WrapFact[Justified, OP] = Justified.wrapFact

  override protected implicit final def wrapSelected: WrapSelected[Justified, OP] = Justified.wrapSelected

  override protected implicit final def wrapQuantifier: WrapQuantifier[Justified, OP] = Justified.wrapQuantifier

}

sealed trait DefinedJustifiedDslImplicitDefinitions extends JustifiedDslTypes {

  protected final val defn: DslImplicitDefinitions[Justified, OP] =
    new DslImplicitDefinitions[Justified, OP]
}

sealed trait JustifiedOutputTypeImplicits
  extends OutputTypeImplicits
  with JustifiedMidPriorityOutputTypeImplicits
  with DefinedJustifiedDslImplicitDefinitions {

  override implicit def constOption[A](
    implicit
    sot: SelectOutputType[Justified, Option[A], A],
    opCO: OP[Option[A]],
  ): ConstOutputType.Aux[Justified, Option[A], Option[sot.Out]] = defn.constTraverse(sot)

  override implicit def constSet[C[a] <: Set[a], A, O](
    implicit
    sot: SelectOutputType.Aux[Justified, C[A], A, O],
    factory: Factory[O, C[O]],
    opCA: OP[C[A]],
  ): ConstOutputType.Aux[Justified, C[A], C[O]] = defn.constIterable(sot)

  override implicit def selectOption[I : OP, A : OP](
    implicit
    sot: SelectOutputType[Justified, I, A],
  ): SelectOutputType.Aux[Justified, I, Option[A], Option[sot.Out]] = defn.selectOption(sot)
}

sealed trait JustifiedMidPriorityOutputTypeImplicits
  extends MidPriorityOutputTypeImplicits
  with JustifiedLowPriorityOutputTypeImplicits
  with DefinedJustifiedDslImplicitDefinitions {

  override implicit final def constTraverse[C[_] : Traverse, O](
    implicit
    sot: SelectOutputType[Justified, C[O], O],
    opCO: OP[C[O]],
  ): ConstOutputType.Aux[Justified, C[O], C[sot.Out]] = defn.constTraverse(sot)

  override implicit final def selectTraverse[C[_] : Traverse, I : OP, O : OP](
    implicit
    sot: SelectOutputType[Justified, I, O],
    nt: C[O] <:!< Product,
  ): SelectOutputType.Aux[Justified, I, C[O], C[sot.Out]] = defn.selectTraverse(sot)
}

sealed trait JustifiedLowPriorityOutputTypeImplicits
  extends LowPriorityOutputTypeImplicits
  with DefinedJustifiedDslImplicitDefinitions {

  override implicit final def constId[O : Wrappable : OP]: ConstOutputType.Aux[Justified, O, Justified[O]] =
    defn.constId

  override implicit final def selectId[
    I : OP,
    O : Wrappable : OP,
  ]: SelectOutputType.Aux[Justified, I, O, Justified[O]] =
    defn.selectId
}

sealed trait JustifiedExprHListDslImplicits
  extends WrappedExprHListDslImplicits
  with JustifiedLowPriorityExprHListDslImplicits
  with DefinedJustifiedDslImplicitDefinitions {

  override implicit def hlastAlignIterableOnceMapN[H](
    implicit
    isCons: IsExprHCons.Aux[IterableOnce[Justified[H]] :: HNil, IterableOnce[Justified[H]], HNil],
  ): ZipToShortest.Aux[Lambda[a => Seq[Justified[a]]], IterableOnce[Justified[H]] :: HNil, OP, H :: HNil] =
    defn.hlastAlignIterableOnceMapN

  override implicit def hlastAlignMapN[C[_] : Functor, H](
    implicit
    isCons: IsExprHCons.Aux[C[Justified[H]] :: HNil, C[Justified[H]], HNil],
  ): ZipToShortest.Aux[Lambda[a => C[Justified[a]]], C[Justified[H]] :: HNil, OP, H :: HNil] =
    defn.hlastAlignMapN

  override implicit def hconsAlignIterableOnceMapN[H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[IterableOnce[Justified[H]] :: WT, IterableOnce[Justified[H]], WT],
    mt: ZipToShortest[Lambda[a => Seq[Justified[a]]], WT, OP],
  ): ZipToShortest.Aux[Lambda[a => Seq[Justified[a]]], IterableOnce[Justified[H]] :: WT, OP, H :: mt.UL] =
    defn.hconsAlignIterableOnceMapN(mt)

  override implicit def hconsAlignMapN[C[_] : Align : FunctorFilter, H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[C[Justified[H]] :: WT, C[Justified[H]], WT],
    mt: ZipToShortest[Lambda[a => C[Justified[a]]], WT, OP],
  ): ZipToShortest.Aux[Lambda[a => C[Justified[a]]], C[Justified[H]] :: WT, OP, H :: mt.UL] =
    defn.hconsAlignMapN(mt)
}

sealed trait JustifiedLowPriorityExprHListDslImplicits
  extends WrappedLowPriorityExprHListDslImplicits
  with DefinedJustifiedDslImplicitDefinitions {

  override implicit final def hlastMapN[H](
    implicit
    isCons: IsExprHCons.Aux[Justified[H] :: HNil, Justified[H], HNil],
  ): ZipToShortest.Aux[Justified, Justified[H] :: HNil, OP, H :: HNil] = defn.hlastMapN

  override implicit final def hconsMapN[H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[Justified[H] :: WT, Justified[H], WT],
    mt: ZipToShortest[Justified, WT, OP],
  ): ZipToShortest.Aux[Justified, Justified[H] :: WT, OP, H :: mt.UL] = defn.hconsMapN(mt)
}
