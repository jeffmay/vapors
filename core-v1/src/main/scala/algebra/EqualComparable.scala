package com.rallyhealth.vapors.v1

package algebra

import cats.{Eq, Functor, Id, Semigroupal}

trait EqualComparable[F[_], V] {

  def isEqual(
    left: F[V],
    right: F[V],
  ): F[Boolean]
}

object EqualComparable extends LowPriorityEqualComparable {

  implicit def eq[V : Eq]: EqualComparable[Id, V] = Eq[V].eqv
}

sealed trait LowPriorityEqualComparable {

  implicit def semigroupalFunctorEq[F[_] : Functor : Semigroupal, V : Eq]: EqualComparable[F, V] = { (left, right) =>
    import cats.syntax.apply._
    (left, right).mapN(Eq[V].eqv)
  }
}
