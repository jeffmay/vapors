package com.rallyhealth.vapors.v1.dsl

import cats.Traverse
import shapeless.<:!<

import scala.collection.Factory

trait OutputTypeImplicits {
  self: DslTypes with MidPriorityOutputTypeImplicits =>

  implicit def constOption[A](
    implicit
    sot: SelectOutputType[W, Option[A], A],
    opCO: OP[Option[A]],
  ): ConstOutputType.Aux[W, Option[A], Option[sot.Out]]

  implicit def constSet[C[a] <: Set[a], A, O](
    implicit
    sot: SelectOutputType.Aux[W, C[A], A, O],
    factory: Factory[O, C[O]],
    opCA: OP[C[A]],
  ): ConstOutputType.Aux[W, C[A], C[O]]

  implicit def selectOption[I : OP, A : OP](
    implicit
    sot: SelectOutputType[W, I, A],
  ): SelectOutputType.Aux[W, I, Option[A], Option[sot.Out]]
}

trait MidPriorityOutputTypeImplicits {
  self: DslTypes with LowPriorityOutputTypeImplicits =>

  implicit def constTraverse[C[_] : Traverse, O](
    implicit
    sot: SelectOutputType[W, C[O], O],
    opCO: OP[C[O]],
  ): ConstOutputType.Aux[W, C[O], C[sot.Out]]

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

  implicit def constId[O : Wrappable : OP]: ConstOutputType.Aux[W, O, W[O]]

  implicit def selectId[I : OP, O : Wrappable : OP]: SelectOutputType.Aux[W, I, O, W[O]]
}
