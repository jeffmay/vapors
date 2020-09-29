package com.rallyhealth.vapors.factfilter.dsl

private[dsl] trait Syntax {

  def >[T : Ordering](lowerBound: T): CondExp[T] = greaterThan(lowerBound)
  def >=[T : Ordering](lowerBound: T): CondExp[T] = greaterThanOrEqual(lowerBound)
  def <[T : Ordering](upperBound: T): CondExp[T] = lessThan(upperBound)
  def <=[T : Ordering](upperBound: T): CondExp[T] = lessThanOrEqual(upperBound)

  import scala.language.implicitConversions
  implicit def logicalOps[T, A](exp: Exp[T, A]): LogicalOps[T, A] = new LogicalOps(exp)
}
