package com.rallyhealth.vapors.v1

package algebra

import data.Justified

import cats.Id

trait WrapConst[F[_]] {

  def wrapConst[A](value: A): F[A]
}

object WrapConst {

  @inline final def apply[F[_] : WrapConst]: WrapConst[F] = implicitly

  implicit val identity: WrapConst[Id] = new WrapConst[Lambda[a => a]] {
    override def wrapConst[A](value: A): A = value
  }

  implicit val justified: WrapConst[Justified] = new WrapConst[Justified] {
    override def wrapConst[A](value: A): Justified[A] = Justified.byConst(value)
  }
}
