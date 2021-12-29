package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import time.CountTime

import shapeless._

import java.time.{Clock, Instant, LocalDate}

/**
  * Provides common functions work working with time.
  *
  * This works by providing an escape hatch for evaluating a custom Scala function (rather than building
  * all the operations out of [[Expr]] nodes).
  *
  * @see [[Expr.CustomFunction]]
  */
trait TimeFunctions {
  self: DslTypes =>

  protected def wrapConst: WrapConst[W, OP]

  /**
    * Computes the difference between the `lhsExpr` and the `rhsExpr` as an integral value of the given unit
    * (can be negative). The result will be rounded down to the maximum number of whole units between the two
    * temporal instances that can be counted.
    *
    * This requires an instance of [[CountTime]] to make sure that the given unit type is safe to count for
    * the given temporal types. This is a safer API than working directly with Java's Time library because it
    * forbids subtracting two different temporal types from each other, without first converting them appropriately.
    * Java Time will just throw an exception if the types are incompatible.
    *
    * @param lhsExpr an expression that computes the temporal instant of when to start counting
    * @param rhsExpr an expression that computes the temporal instant of when to stop counting
    *                (either counting units of time in the past or future from lhsExpr)
    * @param roundToUnitExpr the type of unit to round down to
    */
  def dateDiff[I, T, U](
    lhsExpr: I ~:> W[T],
    rhsExpr: I ~:> W[T],
    roundToUnitExpr: I ~:> W[U],
  )(implicit
    countTime: CountTime[W[T], W[U], W[Long]],
    opL: OP[W[T] :: W[T] :: W[U] :: HNil],
    opO: OP[W[Long]],
  ): AndThen[I, W[T] :: W[T] :: W[U] :: HNil, W[Long]] = {
    (lhsExpr :: rhsExpr :: roundToUnitExpr).andThen {
      Expr.CustomFunction[W[T] :: W[T] :: W[U] :: HNil, W[Long], OP]("date_diff", {
        case lhs :: rhs :: unit :: HNil =>
          CountTime.between(lhs, rhs, unit)
      })
    }
  }

  /**
    * Returns an expression that computes the current [[LocalDate]] when it is invoke, using the system
    * [[Clock]] with the default timezone information of host machine / JVM.
    */
  def today(
    implicit
    opC: OP[LocalDate],
    opO: OP[W[LocalDate]],
  ): Any ~:> W[LocalDate] =
    today(Clock.systemDefaultZone())

  /**
    * Returns an expression that computes the current [[LocalDate]] when it is invoked, based on the given [[Clock]].
    */
  def today(
    clock: Clock,
  )(implicit
    opC: OP[LocalDate],
    opO: OP[W[LocalDate]],
  ): Any ~:> W[LocalDate] = {
    Expr.CustomFunction("today", _ => wrapConst.wrapConst(LocalDate.now(clock)))
  }

  /**
    * Returns an expression that computes the current [[Instant]] when it is invoked, using the UTC [[Clock]].
    */
  def now(
    implicit
    opC: OP[Instant],
    opO: OP[W[Instant]],
  ): Any ~:> W[Instant] =
    now(Clock.systemUTC())

  /**
    * Returns an expression that computes the current [[Instant]] when it is invoked, based on the given [[Clock]].
    */
  def now(
    clock: Clock,
  )(implicit
    opC: OP[Instant],
    opO: OP[W[Instant]],
  ): Any ~:> W[Instant] =
    Expr.CustomFunction("now", _ => wrapConst.wrapConst(Instant.now(clock)))

}
