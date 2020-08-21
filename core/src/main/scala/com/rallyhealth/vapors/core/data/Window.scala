package com.rallyhealth.vapors.core.data

import scala.collection.immutable.NumericRange

// TODO: Support a more Ordering-like interface
// TODO: Add a reverse method
sealed trait Window[A] {

  def contains(value: A): Boolean
}

sealed trait KnownWindow[A] extends Window[A] {

  def lowerBoundOpt: Option[A]

  def upperBoundOpt: Option[A]
}

sealed trait UpperBoundedWindow[A] extends KnownWindow[A] {

  def upperBound: A

  override final def upperBoundOpt: Option[A] = Some(upperBound)

}

sealed trait LowerBoundedWindow[A] extends KnownWindow[A] {

  def lowerBound: A

  override final def lowerBoundOpt: Option[A] = Some(lowerBound)

}

sealed trait BoundedWindow[A] extends LowerBoundedWindow[A] with UpperBoundedWindow[A]

object Window {
  import Ordering.Implicits._

  // This shouldn't be needed
  def fromField[A, B](
    fieldName: String,
    getter: A => B
  )(
    window: Window[B]
  ): Window[A] = FromSelectField(fieldName, getter, window)

  def fromRange(range: Range): Window[Int] = {
    Window.between(range.start, includeStart = true, range.end, includeEnd = range.isInclusive)
  }

  def fromRange[A : Ordering](range: NumericRange[A]): Window[A] = {
    Window.between(range.start, includeStart = true, range.end, includeEnd = range.isInclusive)
  }

  def lowerThan[A : Ordering](
    end: A,
    inclusive: Boolean
  ): Window[A] = Before(end, inclusive)

  def lowerThan[A : Ordering](end: A): Window[A] = Before(end, inclusive = false)

  def equalOrLowerThan[A : Ordering](end: A): Window[A] = Before(end, inclusive = true)

  def after[A : Ordering](
    start: A,
    inclusive: Boolean
  ): Window[A] = After(start, inclusive)

  def after[A : Ordering](start: A): Window[A] = After(start, inclusive = false)

  def onOrAfter[A : Ordering](start: A): Window[A] = After(start, inclusive = true)

  def between[A : Ordering](
    start: A,
    includeStart: Boolean,
    end: A,
    includeEnd: Boolean
  ): Window[A] =
    Between(start, includeStart, end, includeEnd)

  def between[A : Ordering](
    start: A,
    end: A
  ): Window[A] =
    Between(start, includeStart = true, end, includeEnd = false)

  def betweenExclusive[A : Ordering](
    start: A,
    end: A
  ): Window[A] =
    Between(start, includeStart = false, end, includeEnd = false)

  def betweenInclusive[A : Ordering](
    start: A,
    end: A
  ): Window[A] =
    Between(start, includeStart = true, end, includeEnd = true)

  def afterUntil[A : Ordering](
    start: A,
    end: A
  ): Window[A] =
    Between(start, includeStart = false, end, includeEnd = true)

  private final case class Before[A : Ordering](
    upperBound: A,
    inclusive: Boolean
  ) extends UpperBoundedWindow[A] {

    override def lowerBoundOpt: Option[A] = None

    private val checkBeforeEnd: A => Boolean = {
      if (inclusive) _ <= upperBound
      else _ < upperBound
    }

    override def contains(value: A): Boolean = checkBeforeEnd(value)
  }

  private final case class After[A : Ordering](
    lowerBound: A,
    inclusive: Boolean
  ) extends LowerBoundedWindow[A] {

    override def upperBoundOpt: Option[A] = None

    private val checkAfterStart: A => Boolean = {
      if (inclusive) _ >= lowerBound
      else _ > lowerBound
    }

    override def contains(value: A): Boolean = checkAfterStart(value)
  }

  protected[Window] final case class Between[A : Ordering](
    lowerBound: A,
    includeStart: Boolean,
    upperBound: A,
    includeEnd: Boolean
  ) extends BoundedWindow[A] {

    private val checkBetween: A => Boolean = (includeStart, includeEnd) match {
      case (true, true) => a => a >= lowerBound && a <= upperBound
      case (true, false) => a => a >= lowerBound && a < upperBound
      case (false, true) => a => a > lowerBound && a <= upperBound
      case (false, false) => a => a > lowerBound && a < upperBound
    }

    override def contains(value: A): Boolean = checkBetween(value)
  }

  // TODO: Remove this and implement a simpler interface
  private final case class FromSelectField[A, B](
    fieldName: String,
    getter: A => B,
    window: Window[B],
  ) extends Window[A] {

    override def contains(value: A): Boolean = window.contains(getter(value))
  }
}
