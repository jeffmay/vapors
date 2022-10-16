package com.rallyhealth.vapors.v1

package dsl

import shapeless3.deriving.{Const, Id}

/**
  * Wraps the given constant with the given wrapper type.
  *
  * Mirrors the [[ConstOutputType]] but implemented for when the appropriate leaf type has been found.
  *
  * This is similar to the `Pure` typeclass from alleycats, except more specific to the Vapors project.
  * It will only be used to construct wrapped constant values.
  *
  * @tparam W the wrapper type to place the constant values into
  */
trait WrapConst[+W[+_], -OP[_]] {

  def wrapConst[A](value: A)(implicit opA: OP[A]): W[A]
}

object WrapConst {

  private object Unwrapped extends WrapConst[[a] =>> a, [_] =>> Any] {
    override def wrapConst[A](value: A)(implicit opA: Any): A = value
  }

  implicit final def unwrapped[OP[_]]: WrapConst[[a] =>> a, OP] = Unwrapped
}
