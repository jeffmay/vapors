package com.rallyhealth.vapors.v1

package logic

import shapeless.Id

import scala.annotation.implicitNotFound

/**
  * Defines logical negation (aka "NOT") for a specific type.
  *
  * @note this is different from defining how to get the negative value of a number.
  */
@implicitNotFound(
  """
Cannot negate a value of type ${W}[${B}] with an output parameter of ${OP}.

To define negation for a custom type (aka the NOT operation), you must define or import an instance of Negation[${W}, ${B}, ${OP}]""",
)
trait Negation[W[_], B, OP[_]] {

  /**
    * Negates the logical interpretation of a given value of type [[B]].
    *
    * In other words, if the value of `W[B]` is "truthy", then the negation of that value must be "falsy" and, likewise,
    * if the value of `W[B]` is "falsy", then the negation of that value must by "truthy." If the given value is neither
    * "truthy" nor "falsy," then this should return that value unchanged. However, you should consider why you want
    * to allow negating a type that contains non-boolean values.
    */
  def not(value: W[B])(implicit opB: OP[W[B]]): W[B]
}

object Negation {

  @inline implicit def bool[OP[_]]: Negation[Id, Boolean, OP] = Logic.bool
}
