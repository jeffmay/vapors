package com.rallyhealth.vapors.core.dsl.factfilter

import com.rallyhealth.vapors.core.dsl

private[dsl] trait Syntax {
  self: Dsl =>

  /**
    * An alias to this [[dsl]] object, so you can use infix operators and more easily explore
    * the list of supported expression builders.
    *
    * Example:
    * {{{
    *   __.withFactsOfType(FactTypes.Age)
    *     .whereAnyValue(__ > 35)
    * }}}
    */
  final val __ = this

  def >[T : Ordering](lowerBound: T): CondExp[T] = greaterThan(lowerBound)
  def >=[T : Ordering](lowerBound: T): CondExp[T] = greaterThanOrEqual(lowerBound)
  def <[T : Ordering](upperBound: T): CondExp[T] = lessThan(upperBound)
  def <=[T : Ordering](upperBound: T): CondExp[T] = lessThanOrEqual(upperBound)

  import scala.language.implicitConversions
  implicit def logicalOps[T, A](exp: AnyExp[T, A]): LogicalOps[T, A] = new LogicalOps(exp)
}
