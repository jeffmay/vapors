package com.rallyhealth.vapors.v1

package algebra

import data.Justified

import shapeless.Id

/**
  * Extract the value from a context `F`.
  *
  * Although this typeclass exists in alleycats, I don't want to force a dependency on it.
  *
  * It is simple enough to implement and it is required at the DSL import level, so it won't
  * be implemented very often by end users.
  */
trait Extract[F[_]] {

  def extract[A](fa: F[A]): A
}

object Extract {

  @inline final def apply[F[_] : Extract]: Extract[F] = implicitly

  implicit val identity: Extract[Id] = new Extract[Id] {
    override def extract[A](fa: A): A = fa
  }

  implicit val justified: Extract[Justified] = new Extract[Justified] {
    override def extract[A](fa: Justified[A]): A = fa.value
  }
}
