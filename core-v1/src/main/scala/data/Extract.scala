package com.rallyhealth.vapors.v1

package data

import cats.Id

/**
  * Extract the value from a context [[W]].
  *
  * Although this typeclass exists in alleycats, I don't want to force a dependency on it.
  * It is simple enough to implement and it is required at the DSL import level, so it won't
  * be implemented very often by end users. Also it may need to evolve in a manner that doesn't
  * align with the cats typeclass.
  */
trait Extract[W[_]] {

  def extract[A](fa: W[A]): A
}

object Extract {

  @inline final def apply[W[_] : Extract]: Extract[W] = implicitly

  implicit val identity: Extract[Id] = new Extract[Id] {
    override def extract[A](fa: A): A = fa
  }
}
