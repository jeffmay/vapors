package com.rallyhealth.vapors.v1

package dsl

import com.rallyhealth.vapors.v1.lens.DataPath

import scala.annotation.implicitNotFound

/**
  * The type-level definition for the computation required for the [[WrapSelected]] operation.
  *
  * @note This does not require the [[DslTypes.OP]] type because that is provided by the DSL.
  *       This trait simply defines the shell of how to compute the output type, whereas the actual
  *       implementation will require the implicit parameters.
  *
  * @tparam W the wrapper (or effect) type
  * @tparam I the original unwrapped input type
  * @tparam A the type selected by the lens
  * @return the appropriately wrapped output scalar or functor type
  */
@implicitNotFound(
  """
Cannot find the appropriate output type when selecting a value of type ${A} from ${I}.

Typically, this means that you are calling .select() and creating a lens to a non-Functor higher-kinded type.""",
)
trait SelectOutputType[W[+_], I, A] {
  type Out

  /**
    * This mirrors the [[WrapSelected.wrapSelected]] interface, but the final output type is deferred
    * to the appropriate definition (as determined by implicit resolution).
    */
  def wrapSelected(
    wrapped: W[I],
    path: DataPath,
    value: A,
  ): Out
}

object SelectOutputType {
  type Aux[W[+_], I, A, B] = SelectOutputType[W, I, A] { type Out = B }
}
