package com.rallyhealth.vapors.v1

package dsl

import com.rallyhealth.vapors.v1.lens.DataPath
import shapeless.Id

/**
  * Wraps an element that was selected from the same wrapper type.
  *
  * This function is invoked after a [[lens.VariantLens]] is applied to the element extracted from the wrapper.
  *
  * Mirrors the [[SelectOutputType]] but implemented for when the appropriate leaf type has been found.
  *
  * @tparam W the wrapper type from which the element was selected.
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[DslTypes.OP]] for more details.
  */
trait WrapSelected[W[+_], OP[_]] {

  /**
    * Wrap the selected element with the defined wrapper type [[W]].
    *
    * @param container the wrapped value from which the element was selected
    * @param element the element selected from the given container
    * @return the selected element wrapped as necessary
    */
  def wrapSelected[I, O](
    container: W[I],
    path: DataPath,
    element: O,
  )(implicit
    opA: OP[I],
    opB: OP[O],
  ): W[O]
}

object WrapSelected {

  private final object Unwrapped extends WrapSelected[Id, Any] {
    override def wrapSelected[A, B](
      container: A,
      path: DataPath,
      element: B,
    )(implicit
      opA: Any,
      opB: Any,
    ): B = element
  }

  implicit def unwrapped[OP[_]]: WrapSelected[Id, OP] = Unwrapped.asInstanceOf[WrapSelected[Id, OP]]
}
