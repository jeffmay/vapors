package com.rallyhealth.vapors.core.data

import cats.data.Ior
import cats.{Contravariant, Invariant, Show}

import scala.collection.immutable.NumericRange
import scala.reflect.{classTag, ClassTag}

sealed trait Window[A] {
  def contains(value: A): Boolean
}

trait UnknownWindow[A] extends Window[A]

trait BoundedWindow[A] extends Window[A] {
  def bounds: Ior[Bounded.Above[A], Bounded.Below[A]]
}

object Window {
  import Bounded._
  import cats.syntax.functor._
  import cats.syntax.show._
  import Ordering.Implicits._

  def showBoundedWithTerm[A : Show](term: String): Show[BoundedWindow[A]] = Show.show { window =>
    import cats.instances.tuple._
    import cats.instances.string._
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

  implicit def showBounded[A : Show]: Show[BoundedWindow[A]] = showBoundedWithTerm("x")

  implicit final object BoundedInstances extends Invariant[BoundedWindow] {
    override def imap[A, B](fa: BoundedWindow[A])(f: A => B)(g: B => A): BoundedWindow[B] = {
      KnownWindow(fa.bounds.bimap(a => a.map(f), b => b.map(f)))(Ordering.by(g.andThen(fa.contains)))
    }
  }

  implicit def showWindow[A : ClassTag : Show]: Show[Window[A]] = Show.show {
    case window: BoundedWindow[A] => window.show
    case window: UnknownWindow[A] => window.show
  }

  implicit final object UnknownInstances extends Contravariant[UnknownWindow] {
    override def contramap[A, B](fa: UnknownWindow[A])(f: B => A): UnknownWindow[B] = { b =>
      fa.contains(f(b))
    }
  }

  implicit def showUnknown[A : ClassTag]: Show[UnknownWindow[A]] = Show.show { window =>
    s"($window : UnknownWindow[${classTag[A].runtimeClass.getSimpleName}])"
  }

  def fromRange(range: Range): Window[Int] = {
    Window.between(range.start, includeMin = true, range.end, includeMax = range.isInclusive)
  }

  def fromRange[A : Ordering](range: NumericRange[A]): Window[A] = {
    Window.between(range.start, includeMin = true, range.end, includeMax = range.isInclusive)
  }

  def lessThan[A : Ordering](
    max: A,
    inclusive: Boolean,
  ): Window[A] = KnownWindow(Ior.Right(Below(max, inclusive)))

  @inline def lessThan[A : Ordering](max: A): Window[A] = lessThan(max, inclusive = false)

  @inline def lessThanOrEqual[A : Ordering](max: A): Window[A] = lessThan(max, inclusive = true)

  def greaterThan[A : Ordering](
    min: A,
    inclusive: Boolean,
  ): Window[A] = KnownWindow(Ior.Left(Above(min, inclusiveLowerBound = inclusive)))

  @inline def greaterThan[A : Ordering](min: A): Window[A] = greaterThan(min, inclusive = false)

  @inline def greaterThanOrEqual[A : Ordering](min: A): Window[A] = greaterThan(min, inclusive = true)

  def between[A : Ordering](
    min: A,
    includeMin: Boolean,
    max: A,
    includeMax: Boolean,
  ): Window[A] =
    KnownWindow(Ior.Both(Above(min, includeMin), Below(max, includeMax)))

  @inline def between[A : Ordering](
    min: A,
    max: A,
  ): Window[A] = between(min, includeMin = true, max, includeMax = false)

  @inline def betweenInclusive[A : Ordering](
    min: A,
    max: A,
  ): Window[A] = between(min, includeMin = true, max, includeMax = true)

  private[Window] final case class KnownWindow[A : Ordering](bounds: Ior[Above[A], Below[A]]) extends BoundedWindow[A] {

    private val checkBetween: A => Boolean = bounds match {
      case Ior.Left(lb) if lb.inclusiveLowerBound => _ >= lb.lowerBound
      case Ior.Left(lb) => _ > lb.lowerBound

      case Ior.Right(ub) if ub.inclusiveUpperBound => _ <= ub.upperBound
      case Ior.Right(ub) => _ < ub.upperBound

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
