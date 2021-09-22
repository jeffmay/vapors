package com.rallyhealth.vapors.v1

package data

import cats.Functor

/**
  * Represents a boundary on a 1-dimensional line at a specific position.
  *
  * It has two directions: [[Bounded.Below]] and [[Bounded.Above]].
  */
sealed trait Bounded[+A] {

  def map[B](fn: A => B): Bounded[B]

  def withInclusive(inclusive: Boolean): Bounded[A]
}

object Bounded {

  final case class Above[+A](
    lowerBound: A,
    inclusiveLowerBound: Boolean,
  ) extends Bounded[A] {
    override def map[B](fn: A => B): Above[B] = copy(lowerBound = fn(lowerBound))
    override def withInclusive(inclusive: Boolean): Above[A] = copy(inclusiveLowerBound = inclusive)
  }

  final case class Below[+A](
    upperBound: A,
    inclusiveUpperBound: Boolean,
  ) extends Bounded[A] {
    override def map[B](fn: A => B): Below[B] = copy(upperBound = fn(upperBound))
    override def withInclusive(inclusive: Boolean): Below[A] = copy(inclusiveUpperBound = inclusive)
  }

  implicit final object AboveFunctorLike extends Functor[Above] {
    override def map[A, B](fa: Above[A])(f: A => B): Above[B] =
      fa.copy[B](lowerBound = f(fa.lowerBound))
  }

  implicit final object BelowFunctorLike extends Functor[Below] {
    override def map[A, B](fa: Below[A])(f: A => B): Below[B] =
      fa.copy[B](upperBound = f(fa.upperBound))
  }

}
