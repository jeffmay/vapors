package com.rallyhealth.vapors.v1

package dsl

import shapeless.Id

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
trait WrapConst[W[_]] {

  def wrapConst[A](value: A): W[A]
}

object WrapConst {

  @inline final def apply[W[_] : WrapConst]: WrapConst[W] = implicitly

  def wrap[W[_], V](value: V)(implicit wrap: WrapConst[W]): W[V] = wrap.wrapConst(value)

  implicit val identity: WrapConst[Id] = new WrapConst[Id] {
    override def wrapConst[A](value: A): A = value
  }
}
