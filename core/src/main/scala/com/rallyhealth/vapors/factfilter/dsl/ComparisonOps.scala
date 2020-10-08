package com.rallyhealth.vapors.factfilter.dsl

import cats.Eq

private[dsl] trait ComparisonOps {

  def >[T : Ordering](lowerBound: T): CondExp[T] = greaterThan(lowerBound)
  def >=[T : Ordering](lowerBound: T): CondExp[T] = greaterThanOrEqual(lowerBound)
  def <[T : Ordering](upperBound: T): CondExp[T] = lessThan(upperBound)
  def <=[T : Ordering](upperBound: T): CondExp[T] = lessThanOrEqual(upperBound)
  def ===[T : Eq](value: T): CondExp[T] = equalTo(value)
}
