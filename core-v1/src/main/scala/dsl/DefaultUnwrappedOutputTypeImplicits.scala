package com.rallyhealth.vapors.v1.dsl

import cats.Traverse
// TODO: Replace the need for this
//import shapeless.<:!<

import scala.collection.Factory

trait DefaultUnwrappedOutputTypeImplicits
  extends OutputTypeImplicits
  with DefaultUnwrappedMidPriorityOutputTypeImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  override implicit final def constOption[A](
    implicit
    sot: SelectOutputType[W, Option[A], A],
    opCO: OP[Option[A]],
  ): ConstOutputType.Aux[W, Option[A], Option[sot.Out]] = defn.constTraverse(sot)

  override implicit final def constSet[C[a] <: Set[a], A, O](
    implicit
    sot: SelectOutputType.Aux[W, C[A], A, O],
    factory: Factory[O, C[O]],
    opCA: OP[C[A]],
  ): ConstOutputType.Aux[W, C[A], C[O]] = defn.constIterable(sot)

  override implicit final def selectOption[I : OP, A : OP](
    implicit
    sot: SelectOutputType[W, I, A],
  ): SelectOutputType.Aux[W, I, Option[A], Option[sot.Out]] = defn.selectOption(sot)
}

trait DefaultUnwrappedMidPriorityOutputTypeImplicits
  extends MidPriorityOutputTypeImplicits
  with DefaultUnwrappedLowPriorityOutputTypeImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  override implicit final def constTraverse[C[_] : Traverse, O](
    implicit
    sot: SelectOutputType[W, C[O], O],
    opCO: OP[C[O]],
  ): ConstOutputType.Aux[W, C[O], C[sot.Out]] = defn.constTraverse[C, O](sot)

  override implicit final def selectTraverse[C[_] : Traverse, I : OP, A : OP](
    implicit
    sot: SelectOutputType[W, I, A],
//    nt: C[A] <:!< Product,
  ): SelectOutputType.Aux[W, I, C[A], C[sot.Out]] = defn.selectTraverse[C, I, A, sot.Out](sot)

}

trait DefaultUnwrappedLowPriorityOutputTypeImplicits
  extends LowPriorityOutputTypeImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  override implicit final def constId[O : Wrappable : OP]: ConstOutputType.Aux[W, O, O] = defn.constId

  override implicit final def selectId[I : OP, O : Wrappable : OP]: SelectOutputType.Aux[W, I, O, O] = defn.selectId
}
