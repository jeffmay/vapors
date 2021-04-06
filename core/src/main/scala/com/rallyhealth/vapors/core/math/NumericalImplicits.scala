package com.rallyhealth.vapors.core.math

/**
  * Defines default implementations for [[Numeric]]-based arithmetic operators.
  */
private[math] trait NumericalImplicits {

  implicit def integral[A : Integral]: FromIntegral[A] = new FromIntegral[A]
  implicit def fractional[A : Fractional]: FromFractional[A] = new FromFractional[A]
}

abstract class FromNumeric[A : Numeric] extends Addition[A] with Subtraction[A] with Negative[A] {
  import Numeric.Implicits._

  override def add(
    lhs: A,
    rhs: A,
  ): A = lhs + rhs

  override def subtract(
    lhs: A,
    rhs: A,
  ): A = lhs - rhs

  override def negative(value: A): A = -value
}

/**
  * Defines all arithmetic type-classes from Scala's [[Numeric]] definition.
  */
final class FromIntegral[A : Integral] extends FromNumeric[A] with Division[A] {
  import Integral.Implicits._

  override def quot(
    dividend: A,
    divisor: A,
  ): A = dividend / divisor
}

final class FromFractional[A : Fractional] extends FromNumeric[A] with Division[A] {
  import Fractional.Implicits._

  override def quot(
    dividend: A,
    divisor: A,
  ): A = dividend / divisor
}
