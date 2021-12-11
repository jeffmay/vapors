package com.rallyhealth.vapors.v1

package logic

import shapeless.Id

import scala.annotation.implicitNotFound

/**
  * Combines all the logical operations (`AND` / `OR` / `NOT`) into a single trait to make it easier
  * to define without having to pass 3 type parameters to 3 separate traits.
  *
  * @tparam F the wrapper (or effect) type over which equality is computed
  * @tparam B the boolean-like value type to perform the logical operations
  * @tparam OP a custom output parameter type used by visitors to enable post-processing operations.
  *            See [[dsl.DslTypes.OP]] for more details.
  */
@implicitNotFound(
  """Cannot perform a logical operations (like AND / OR / NOT) on values of type ${F}[${B}] with an output parameter of ${OP}. 
To define the operations, you must define or import an instance of Logic[${F}, ${B}, ${OP}]""",
)
trait Logic[F[_], B, OP[_]] extends Conjunction[F, B, OP] with Disjunction[F, B, OP] with Negation[F, B, OP]

object Logic {

  @inline implicit final def bool[OP[_]]: Logic[Id, Boolean, OP] = AnyBool.asInstanceOf[Logic[Id, Boolean, OP]]

  private final object AnyBool extends Logic[Id, Boolean, Any] {

    override def and(
      left: Boolean,
      right: Boolean,
    )(implicit
      opB: Any,
    ): Boolean = left && right

    override def or(
      left: Boolean,
      right: Boolean,
    )(implicit
      opB: Any,
    ): Boolean = left || right

    override def not(value: Boolean)(implicit opB: Any): Boolean = !value
  }

}
