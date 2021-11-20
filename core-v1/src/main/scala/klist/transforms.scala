package com.rallyhealth.vapors.v1

package klist

import cats.{~>, Foldable, Id}

object transforms {

  trait Down[C[_]] extends (C ~> Id) {
    override def apply[A](fa: C[A]): A
  }

}
