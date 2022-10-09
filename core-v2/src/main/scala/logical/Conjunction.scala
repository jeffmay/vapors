package com.rallyhealth.vapors.v2
package logical

import optical.Extracting

import zio.prelude.*

trait Conjunction[A] extends BooleanLike[A] {
  def conjunction[F[+_] : CommutativeBoth : Covariant](one: F[A], two: F[A]): F[A]
}

object Conjunction {

  inline final def apply[A : Conjunction]: Conjunction[A] = summon

  private final class FromFunction[A : Extracting[Boolean]](fn: (A, A) => A) extends BooleanLike.ByExtracting[A]
    with Conjunction[A] {
    override def conjunction[F[+_] : CommutativeBoth : Covariant](one: F[A], two: F[A]): F[A] =
      (one, two).mapParN(fn)
  }

  def fromFunction[A : Extracting[Boolean]](fn: (A, A) => A): Conjunction[A] = FromFunction[A](fn)

  given boolean: Conjunction[Boolean] = fromFunction(_ && _)
}
