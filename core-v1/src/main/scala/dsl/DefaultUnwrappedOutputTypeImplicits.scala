package com.rallyhealth.vapors.v1.dsl

import cats.Traverse
import shapeless.<:!<

trait DefaultUnwrappedOutputTypeImplicits
  extends OutputTypeImplicits
  with DefaultUnwrappedMidPriorityOutputTypeImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  implicit def constTraverse[C[_] : Traverse, O](
    implicit
    sot: SelectOutputType[W, C[O], O],
    opCO: OP[C[O]],
  ): ConstOutputType.Aux[W, C[O], C[sot.Out]] = defn.constTraverse[C, O](sot)

  implicit final def selectOption[I : OP, O : OP](
    implicit
    sot: SelectOutputType[W, I, O],
  ): SelectOutputType.Aux[W, I, Option[O], Option[sot.Out]] = defn.selectOption(sot)

}

trait DefaultUnwrappedMidPriorityOutputTypeImplicits
  extends MidPriorityOutputTypeImplicits
  with DefaultUnwrappedLowPriorityOutputTypeImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  override implicit final def selectTraverse[C[_] : Traverse, I : OP, O : OP](
    implicit
    sot: SelectOutputType[W, I, O],
    nt: C[O] <:!< Product,
  ): SelectOutputType.Aux[W, I, C[O], C[sot.Out]] = defn.selectTraverse[C, I, O](sot)

}

trait DefaultUnwrappedLowPriorityOutputTypeImplicits
  extends LowPriorityOutputTypeImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  override implicit final def selectId[I : OP, O : OP]: SelectOutputType.Aux[W, I, O, O] = defn.selectId

  override implicit final def constId[O : OP]: ConstOutputType.Aux[W, O, O] = defn.constId
}
