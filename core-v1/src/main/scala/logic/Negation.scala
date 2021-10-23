package com.rallyhealth.vapors.v1

package logic

import cats.{Invariant, Semigroupal}

import scala.annotation.implicitNotFound

/**
  * Defines logical negation (aka "NOT") for a specific type.
  *
  * @note this is different from defining how to get the negative value of a number.
  */
@implicitNotFound(
  "Cannot negate a value of type ${A}. If you want like to define negation (aka the NOT operation) for a custom type, you must implement or import an instance of Negation[${A}]",
)
trait Negation[A] {

  /**
    * Negates the logical interpretation of a given value of type `A`.
    *
    * In other words, if the value of `A` is "truthy", then the negation of that value must be "falsy" and, likewise,
    * if the value of `A` is "falsy", then the negation of that value must by "truthy." If the given value is neither
    * "truthy" nor "falsy," then this should return that value unchanged. However, you should consider why you want
    * to allow negating a type that contains non-boolean values.
    */
  def negation(value: A): A
}

object Negation {

  @inline final def apply[A](implicit A: Negation[A]): Negation[A] = A

  implicit final object BooleanNegation extends Negation[Boolean] {
    override def negation(value: Boolean): Boolean = !value
  }

  implicit final object InvariantInstances extends Invariant[Negation] with Semigroupal[Negation] {

    override def product[A, B](
      fa: Negation[A],
      fb: Negation[B],
    ): Negation[(A, B)] = { value =>
      (fa.negation(value._1), fb.negation(value._2))
    }

    override def imap[A, B](fa: Negation[A])(f: A => B)(g: B => A): Negation[B] = { value =>
      f(fa.negation(g(value)))
    }
  }
}
