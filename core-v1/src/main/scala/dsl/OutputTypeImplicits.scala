package com.rallyhealth.vapors.v1.dsl

import cats.Traverse
import shapeless.<:!<

trait OutputTypeImplicits {
  self: DslTypes with MidPriorityOutputTypeImplicits =>

  implicit def constTraverse[C[_] : Traverse, O](
    implicit
    sot: SelectOutputType[W, C[O], O],
    opCO: OP[C[O]],
  ): ConstOutputType.Aux[W, C[O], C[sot.Out]]

  implicit def selectOption[I : OP, O : OP](
    implicit
    sot: SelectOutputType[W, I, O],
  ): SelectOutputType.Aux[W, I, Option[O], Option[sot.Out]]
}

trait MidPriorityOutputTypeImplicits {
  self: DslTypes with LowPriorityOutputTypeImplicits =>

  /**
    * Recursively select into a traversable type to wrap the leaf node type. This makes it easier to determine
    * the next operation you would want to perform on wrapped collection values.
    *
    * @param nt Proof that this collection is not a Tuple. This is required because cats implements Traverse
    *           for the last element of Tuples, which will cause the wrapper to only wrap the last element of
    *           the [[Product]] type. We want to always wrap the whole product value and use a lens to select
    *           a specific element of the product type.
    */
  implicit def selectTraverse[C[_] : Traverse, I : OP, O : OP](
    implicit
    sot: SelectOutputType[W, I, O],
    nt: C[O] <:!< Product,
  ): SelectOutputType.Aux[W, I, C[O], C[sot.Out]]
}

trait LowPriorityOutputTypeImplicits {
  self: DslTypes =>

  implicit def constId[O : OP]: ConstOutputType.Aux[W, O, W[O]]

  implicit def selectId[I : OP, O : OP]: SelectOutputType.Aux[W, I, O, W[O]]
}
