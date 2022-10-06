package com.rallyhealth.vapors.v1

package data

import cats.data.Ior
import cats.{Order, Show}

import scala.collection.immutable.NumericRange

/**
  * Represents an open or closed range with either 1 or 2 boundaries.
  *
  * A window can be bounded from below or above or both, and it can tell if a value is contained within
  * those boundaries.
  */
sealed trait Window[+A] {

  def bounds: Option[Ior[Bounded.Above[A], Bounded.Below[A]]]

  final def mapBothBounds[B >: A : Order](
    fnLowerBound: A => B,
    fnUpperBound: A => B,
  ): Window[B] =
    bounds.fold(Window.empty[B]) { known =>
      Window.fromBounds {
        known.bimap(_.map(fnLowerBound), _.map(fnUpperBound))
      }
    }

  final def mapUpperBound[B >: A : Order](fn: A => B): Window[B] =
    mapBothBounds(identity, fn)

  final def mapLowerBound[B >: A : Order](fn: A => B): Window[B] =
    mapBothBounds(fn, identity)
}

object Window {
  import Bounded._
  import cats.syntax.order._

  @inline final def empty[A]: Window[A] = Empty

  private case object Empty extends Window[Nothing] {
    override def bounds: Option[Ior[Above[Nothing], Below[Nothing]]] = None
  }

  // TODO: Use extension methods
  implicit final class IWindowOps[A](private val window: Window[A]) extends AnyVal {

    def contains(value: A): Boolean = window match {
      case Empty => false
      case knownWindow @ KnownWindow(_) => knownWindow.contains(value)
    }
  }

  def showWindowWithTerm[A : Show](term: String): Show[Window[A]] = Show.show {
    case Empty => "Window.Empty"
    case KnownWindow(bounds) =>
      val (op1, op2) = bounds
        .bimap(
          b => (if (b.inclusiveLowerBound) ">=" else ">", ""),
          b => ("", if (b.inclusiveUpperBound) "<=" else "<"),
        )
        .merge
      val showA: A => String = Show[A].show
      bounds match {
        case Ior.Left(lb) => s"$term $op1 ${showA(lb.lowerBound)}"
        case Ior.Right(ub) => s"$term $op2 ${showA(ub.upperBound)}"
        case Ior.Both(lb, ub) =>
          s"Window[$term $op1 ${showA(lb.lowerBound)} and $term $op2 ${showA(ub.upperBound)}]"
      }
  }

  implicit def showWindow[A : Show]: Show[Window[A]] = showWindowWithTerm("x")

  def fromBounds[A : Order](bounds: Ior[Bounded.Above[A], Bounded.Below[A]]): Window[A] = KnownWindow(bounds)

  def fromRange(range: Range): Window[Int] =
    between(range.start, includeMin = true, range.end, includeMax = range.isInclusive)

  def fromRange[A : Order](range: NumericRange[A]): Window[A] =
    between(range.start, includeMin = true, range.end, includeMax = range.isInclusive)

  def equalTo[A : Order](value: A): Window[A] =
    betweenInclusive(value, value)

  def lessThan[A : Order](
    max: A,
    inclusive: Boolean,
  ): Window[A] = KnownWindow(Ior.Right(Below(max, inclusive)))

  /** (-infinity, max) */
  def lessThan[A : Order](max: A): Window[A] = lessThan(max, inclusive = false)

  /** (-infinity, max] */
  def lessThanOrEqual[A : Order](max: A): Window[A] = lessThan(max, inclusive = true)

  /** Generally defined range with configurable open or lower bound. */
  def greaterThan[A : Order](
    min: A,
    inclusive: Boolean,
  ): Window[A] = KnownWindow(Ior.Left(Above(min, inclusiveLowerBound = inclusive)))

  /** (min, +infinity) */
  def greaterThan[A : Order](min: A): Window[A] = greaterThan(min, inclusive = false)

  /** [min, +infinity) */
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

  private final case class KnownWindow[A : Order](knownBounds: Ior[Above[A], Below[A]]) extends Window[A] {

    override def bounds: Option[Ior[Above[A], Below[A]]] = Some(knownBounds)

    private val checkBetween: A => Boolean = knownBounds match {
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

    def contains(value: A): Boolean = checkBetween(value)
  }

}
