package com.rallyhealth

package vapors.data

import alleycats.Empty
import cats.data.Ior
import cats.{Invariant, Order, Show}

import scala.collection.immutable.NumericRange

/**
  * Represents an open or closed range with either 1 or 2 boundaries.
  *
  * A window can be bounded from below or above or both, and it can tell if a value is contained within
  * those boundaries.
  */
trait Window[A] {
  def order: Order[A]
  def contains(value: A): Boolean
  def bounds: Ior[Bounded.Above[A], Bounded.Below[A]]
}

object Window {
  import Bounded._
  import cats.syntax.functor._
  import cats.syntax.order._
  import cats.syntax.show._

  def empty[A : Order : Empty]: Window[A] = new EmptyWindow[A]

  private final class EmptyWindow[A : Order : Empty] extends Window[A] {
    private val zero = Empty[A].empty
    override val order: Order[A] = Order[A]
    override def contains(value: A): Boolean = false
    override def bounds: Ior[Above[A], Below[A]] =
      Ior.Both(Above(zero, inclusiveLowerBound = false), Below(zero, inclusiveUpperBound = false))
    override def toString: String = s"Window.empty($zero)"
  }

  implicit object InvariantWindow extends Invariant[Window] {
    override def imap[A, B](fa: Window[A])(f: A => B)(g: B => A): Window[B] = {
      implicit val o: Order[B] = Order.by(g)(fa.order)
      val newBounds = fa.bounds.bimap(a => a.map(f), b => b.map(f))
      KnownWindow(newBounds)
    }
  }

  def showWindowWithTerm[A : Show](term: String): Show[Window[A]] = Show.show { window =>
    val (op1, op2) = window.bounds
      .bimap(
        b => (if (b.inclusiveLowerBound) ">=" else ">", ""),
        b => ("", if (b.inclusiveUpperBound) "<=" else "<"),
      )
      .merge
    window.bounds match {
      case Ior.Left(lb) => s"$term $op1 ${lb.lowerBound.show}"
      case Ior.Right(ub) => s"$term $op2 ${ub.upperBound.show}"
      case Ior.Both(lb, ub) =>
        s"$term $op1 ${lb.lowerBound.show} and $term $op2 ${ub.upperBound.show}"
    }
  }

  implicit def showWindow[A : Show]: Show[Window[A]] = showWindowWithTerm("x")

  def fromBounds[A : Order](bounds: Ior[Bounded.Above[A], Bounded.Below[A]]): Window[A] = KnownWindow(bounds)

  def fromRange(range: Range): Window[Int] =
    Window.between(range.start, includeMin = true, range.end, includeMax = range.isInclusive)

  def fromRange[A : Order](range: NumericRange[A]): Window[A] =
    Window.between(range.start, includeMin = true, range.end, includeMax = range.isInclusive)

  def equalTo[A : Order](value: A): Window[A] =
    Window.betweenInclusive(value, value)

  def lessThan[A : Order](
    max: A,
    inclusive: Boolean,
  ): Window[A] = KnownWindow(Ior.Right(Below(max, inclusive)))

  def lessThan[A : Order](max: A): Window[A] = lessThan(max, inclusive = false)

  def lessThanOrEqual[A : Order](max: A): Window[A] = lessThan(max, inclusive = true)

  def greaterThan[A : Order](
    min: A,
    inclusive: Boolean,
  ): Window[A] = KnownWindow(Ior.Left(Above(min, inclusiveLowerBound = inclusive)))

  def greaterThan[A : Order](min: A): Window[A] = greaterThan(min, inclusive = false)

  def greaterThanOrEqual[A : Order](min: A): Window[A] = greaterThan(min, inclusive = true)

  /** Generally defined bounded range with the ability to set open or closed at each end of the range.*/
  def between[A : Order](
    min: A,
    includeMin: Boolean,
    max: A,
    includeMax: Boolean,
  ): Window[A] =
    KnownWindow(Ior.Both(Above(min, includeMin), Below(max, includeMax)))

  /** [min,max) */
  def between[A : Order](
    min: A,
    max: A,
  ): Window[A] = between(min, includeMin = true, max, includeMax = false)

  /** [min,max] */
  def betweenInclusive[A : Order](
    min: A,
    max: A,
  ): Window[A] = between(min, includeMin = true, max, includeMax = true)

  private[Window] final case class KnownWindow[A](
    bounds: Ior[Above[A], Below[A]],
  )(implicit
    override val order: Order[A],
  ) extends Window[A] {

    private val checkBetween: A => Boolean = bounds match {
      case Ior.Left(lb) if lb.inclusiveLowerBound => _ >= lb.lowerBound
      case Ior.Left(lb) => _ > lb.lowerBound

      case Ior.Right(ub) if ub.inclusiveUpperBound => _ <= ub.upperBound
      case Ior.Right(ub) => _ < ub.upperBound

      case Ior.Both(lb, ub) if lb.lowerBound == ub.upperBound && (lb.inclusiveLowerBound || ub.inclusiveUpperBound) =>
        a => a == lb.lowerBound
      case Ior.Both(lb, ub) if lb.inclusiveLowerBound && ub.inclusiveUpperBound =>
        a => a >= lb.lowerBound && a <= ub.upperBound
      case Ior.Both(lb, ub) if ub.inclusiveUpperBound =>
        a => a > lb.lowerBound && a <= ub.upperBound
      case Ior.Both(lb, ub) if lb.inclusiveLowerBound =>
        a => a >= lb.lowerBound && a < ub.upperBound
      case Ior.Both(lb, ub) =>
        a => a > lb.lowerBound && a < ub.upperBound
    }

    override def contains(value: A): Boolean = checkBetween(value)
  }

}
