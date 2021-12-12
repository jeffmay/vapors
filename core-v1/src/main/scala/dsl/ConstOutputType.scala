package com.rallyhealth.vapors.v1

package dsl

import scala.annotation.implicitNotFound

/**
  * The type-level definition for the computation required for the [[WrapConst]] operation.
  *
  * @note This does not require the [[DslTypes.OP]] type because that is provided by the DSL.
  *       This trait simply defines the shell of how to compute the output type, whereas the actual
  *       implementation will require the implicit parameters.
  *
  * @tparam W the wrapper (or effect) type
  * @tparam O the original unwrapped input type
  * @return the appropriately wrapped output scalar or functor type
  */
@implicitNotFound("""
Cannot find the appropriate output type when attempting to wrap a value of type ${I} into a constant.

Typically, this means that you are attempting to call .const on a non-Functor higher-kinded type.""")
trait ConstOutputType[W[+_], O] {
  type Out

  def wrapConst(value: O): Out
}

object ConstOutputType {
  type Aux[W[+_], O, WO] = ConstOutputType[W, O] { type Out = WO }
}
