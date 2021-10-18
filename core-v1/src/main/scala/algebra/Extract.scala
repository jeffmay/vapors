package com.rallyhealth.vapors.v1

package algebra

import data.Justified

import cats.Id

// TODO: Replace with cats Monoid or ZIO Prelude?
trait Extract[F[_]] {

  def extract[A](fa: F[A]): A
}

object Extract {

  @inline final def apply[F[_] : Extract]: Extract[F] = implicitly

  implicit val identity: Extract[Id] = new Extract[Lambda[a => a]] {
    override def extract[A](fa: A): A = fa
  }

  implicit val justified: Extract[Justified] = new Extract[Justified] {
    override def extract[A](fa: Justified[A]): A = fa.value
  }
}
