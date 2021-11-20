package com.rallyhealth.vapors.v1

package dsl

import algebra._
import data.{Extract, ExtractValue, Justified}
import logic.Logic

import cats.{Functor, Semigroupal, Traverse}
import shapeless.<:!<

trait JustifiedBuildExprDsl extends WrappedExprDsl with WrapJustifiedImplicits with JustifiedDslTypes {

  override protected implicit final def boolLogic: Logic[Justified, Boolean, OP] = Justified.bool[OP]

  override protected implicit final def windowComparable: WindowComparable[Justified, OP] = WindowComparable.justified

  override protected implicit final def extract: Extract[Justified] = Justified.extract

  override protected implicit final def extractBool[
    B : ExtractValue.AsBoolean,
  ]: ExtractValue.AsBoolean[Justified[B]] = { b =>
    ExtractValue.asBoolean(b.value)
  }

  override protected implicit final def functor: Functor[Justified] = Justified.traverse

  override protected implicit final def semigroupal: Semigroupal[Justified] = Justified.semigroupal

  override protected implicit final def wrapConst: WrapConst[Justified, OP] = Justified.wrapConst

  override protected implicit final def wrapFact: WrapFact[Justified, OP] = Justified.wrapFact

  override protected implicit final def wrapSelected: WrapSelected[Justified, OP] = Justified.wrapSelected

  override protected implicit final def wrapQuantifier: WrapQuantifier[Justified, OP] = Justified.wrapQuantifier

  override protected final val defn: WrapDefinitions[Justified, OP] = new WrapDefinitions[Justified, OP]
}

sealed trait WrapJustifiedImplicits extends WrapImplicits with MidPriorityJustifiedWrapImplicits {

  override implicit def constTraverse[C[_] : Traverse, O](
    implicit
    sot: SelectOutputType[Justified, C[O], O],
    opCO: OP[C[O]],
  ): ConstOutputType.Aux[Justified, C[O], C[sot.Out]] = defn.constTraverse(sot)

  override implicit def selectOption[I : OP, O : OP](
    implicit
    sot: SelectOutputType[Justified, I, O],
  ): SelectOutputType.Aux[Justified, I, Option[O], Option[sot.Out]] = defn.selectOption(sot)
}

sealed trait MidPriorityJustifiedWrapImplicits extends MidPriorityWrapImplicits with LowPriorityJustifiedWrapImplicits {

  override implicit final def selectTraverse[C[_] : Traverse, I : OP, O : OP](
    implicit
    sot: SelectOutputType[Justified, I, O],
    nt: C[O] <:!< Product,
  ): SelectOutputType.Aux[Justified, I, C[O], C[sot.Out]] = defn.selectTraverse(sot)
}

sealed trait LowPriorityJustifiedWrapImplicits extends LowPriorityWrapImplicits with JustifiedDslTypes {

  override implicit final def constId[O : OP]: ConstOutputType.Aux[Justified, O, Justified[O]] = defn.constId

  override implicit final def selectId[I : OP, O : OP]: SelectOutputType.Aux[Justified, I, O, Justified[O]] =
    defn.selectId
}
