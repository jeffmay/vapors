package com.rallyhealth.vapors.core.data

import cats.data.Ior
import cats.implicits.catsKernelStdMonoidForString
import cats.{Contravariant, Invariant, Show}

import scala.collection.immutable.NumericRange

sealed trait Window[A] {
  def contains(value: A): Boolean
}

trait UnknownWindow[A] extends Window[A]

trait BoundedWindow[A] extends Window[A] {
  def bounds: Ior[Bounded.Above[A], Bounded.Below[A]]
}

object Window {
  import Bounded._
  import cats.syntax.show._
  import cats.syntax.functor._

  import Ordering.Implicits._

  implicit final object UnknownContravariant extends Contravariant[UnknownWindow] {
    override def contramap[A, B](fa: UnknownWindow[A])(f: B => A): UnknownWindow[B] = { b =>
      fa.contains(f(b))
    }
  }

  implicit final object KnownInvariant extends Invariant[BoundedWindow] {
    override def imap[A, B](fa: BoundedWindow[A])(f: A => B)(g: B => A): BoundedWindow[B] = {
      KnownWindow(fa.bounds.bimap(a => a.map(f), b => b.map(f)))(Ordering.by(g.andThen(fa.contains)))
    }
  }

  def showWindowWithTerm[A : Show](term: String): Show[BoundedWindow[A]] = Show.show { window =>
    import cats.instances.tuple._
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

  implicit def showWindow[A : Show]: Show[BoundedWindow[A]] = showWindowWithTerm("x")

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

  def lessThan[A : Ordering](max: A): Window[A] = KnownWindow(Ior.Right(Below(max, inclusiveUpperBound = false)))

  def lessThanOrEqual[A : Ordering](max: A): Window[A] = KnownWindow(Ior.Right(Below(max, inclusiveUpperBound = true)))

  def greaterThan[A : Ordering](
    min: A,
    inclusive: Boolean,
  ): Window[A] = KnownWindow(Ior.Left(Above(min, inclusiveLowerBound = inclusive)))

  def greaterThan[A : Ordering](min: A): Window[A] = KnownWindow(Ior.Left(Above(min, inclusiveLowerBound = false)))

  def greaterThanOrEqual[A : Ordering](min: A): Window[A] =
    KnownWindow(Ior.Left(Above(min, inclusiveLowerBound = true)))

  def between[A : Ordering](
    min: A,
    includeMin: Boolean,
    max: A,
    includeMax: Boolean,
  ): Window[A] =
    KnownWindow(Ior.Both(Above(min, includeMin), Below(max, includeMax)))

  def between[A : Ordering](
    min: A,
    max: A,
  ): Window[A] =
    KnownWindow(Ior.Both(Above(min, inclusiveLowerBound = true), Below(max, inclusiveUpperBound = false)))

  def betweenInclusive[A : Ordering](
    min: A,
    max: A,
  ): Window[A] =
    KnownWindow(Ior.Both(Above(min, inclusiveLowerBound = true), Below(max, inclusiveUpperBound = true)))

  protected[Window] final case class KnownWindow[A : Ordering](bounds: Ior[Above[A], Below[A]])
    extends BoundedWindow[A] {

    private val checkBetween: A => Boolean = bounds match {
      case Ior.Left(lb) if lb.inclusiveLowerBound => _ >= lb.lowerBound
      case Ior.Left(lb) => _ > lb.lowerBound

      case Ior.Right(ub) if ub.inclusiveUpperBound => _ >= ub.upperBound
      case Ior.Right(ub) => _ > ub.upperBound

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
