package com.rallyhealth.vapors.factfilter

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg
import com.rallyhealth.vapors.factfilter.data.{Facts, ResultSet}

import scala.language.implicitConversions

package object dsl extends FactFilterDsl with Evaluation {

  /**
    * An alias to all of the methods available from this package (with some exceptions).
    */
  final val __ = new FactFilterDsl

  /**
    * Adds support for logical infix operators (like [[and]] / [[or]]) to any expression that supports it.
    */
  implicit def logicalOps[T, A](exp: Exp[T, A]): LogicalOps[T, A] = new LogicalOps(exp)

  /**
    * The root of all expression types.
    *
    * @tparam X the type of input
    * @tparam A the free parameter, used to describe the eventual output of the [[FreeApplicative]] functor
    */
  final type Exp[X, A] = FreeApplicative[ExpAlg[X, *], A]

  /**
    * An expression that terminates into a boolean, used for making a conditional query or filter.
    *
    * Useful for defining sub-expressions to [[WhereBuilder.whereEveryFact]],
    * [[WhereBuilder.whereAnyFactValue]], [[exists]], [[forall]], etc.
    */
  final type CondExp[X] = Exp[X, Boolean]

  /**
    * A top-level expression that handles facts of any type and returns the filtered list of facts
    * without restriction on type.
    */
  final type TerminalFactsExp = Exp[Facts, ResultSet]
}
