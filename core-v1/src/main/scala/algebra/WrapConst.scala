package com.rallyhealth.vapors.v1

package algebra

import data.Justified

import cats.Id

/**
  * This typeclass defines the capability of this wrapper type being used to create wrapped values in a DSL traits.
  *
  * This is similar to the `Pure` typeclass from alleycats, except more specific to the Vapors project.
  * It will only be used to construct wrapped constant values.
  *
  * @tparam F the wrapper type to place the constant values into
  */
trait WrapConst[F[_]] {

  def wrapConst[A](value: A): F[A]
}

object WrapConst {

  @inline final def apply[F[_] : WrapConst]: WrapConst[F] = implicitly

  def wrap[F[_], V](value: V)(implicit wrap: WrapConst[F]): F[V] = wrap.wrapConst(value)

  implicit val identity: WrapConst[Id] = new WrapConst[Lambda[a => a]] {
    override def wrapConst[A](value: A): A = value
  }

  implicit val justified: WrapConst[Justified] = new WrapConst[Justified] {
    override def wrapConst[A](value: A): Justified[A] = Justified.byConst(value)
  }
}
