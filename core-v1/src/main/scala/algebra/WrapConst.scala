package com.rallyhealth.vapors.v1

package algebra

import data.Justified

import shapeless.Id

/**
  * This typeclass defines the capability of this wrapper type being used to create wrapped values in a DSL traits.
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

  implicit val justified: WrapConst[Justified] = new WrapConst[Justified] {
    override def wrapConst[A](value: A): Justified[A] = Justified.byConst(value)
  }
}
