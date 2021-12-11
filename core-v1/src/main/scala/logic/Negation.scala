package com.rallyhealth.vapors.v1

package logic

import cats.{Invariant, Semigroupal}
import shapeless.Id

import scala.annotation.implicitNotFound

/**
  * Defines logical negation (aka "NOT") for a specific type.
  *
  * @note this is different from defining how to get the negative value of a number.
  */
@implicitNotFound(
  """Cannot negate a value of type ${F}[${B}] with an output parameter of ${OP}. 
To define negation for a custom type (aka the NOT operation), you must define or import an instance of Negation[${F}, ${B}, ${OP}]""",
)
trait Negation[F[_], B, OP[_]] {

  /**
    * Negates the logical interpretation of a given value of type [[B]].
    *
    * In other words, if the value of `F[B]` is "truthy", then the negation of that value must be "falsy" and, likewise,
    * if the value of `F[B]` is "falsy", then the negation of that value must by "truthy." If the given value is neither
    * "truthy" nor "falsy," then this should return that value unchanged. However, you should consider why you want
    * to allow negating a type that contains non-boolean values.
    */
  def not(value: F[B])(implicit opB: OP[F[B]]): F[B]
}

object Negation {

  @inline implicit def bool[OP[_]]: Negation[Id, Boolean, OP] = Logic.bool
}
