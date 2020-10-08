package com.rallyhealth.vapors.factfilter

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg
import com.rallyhealth.vapors.factfilter.data.{Facts, FactsOfType, ResultSet, TypedResultSet}

package object dsl extends Dsl with Evaluation {

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
  final val __ = new InfixOps

  import scala.language.implicitConversions
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
    * Useful for defining sub-expressions to [[WhereFactsExpBuilder.whereAllFacts]],
    * [[WhereFactsExpBuilder.whereAnyValue]], [[exists]], [[all]], etc.
    */
  final type CondExp[X] = Exp[X, Boolean]

  /**
    * An expression that operates on a non-empty list of facts but is unbounded in what it returns.
    *
    * @tparam T the type of fact
    * @tparam A the free parameter
    */
  final type FactsExp[T, A] = Exp[FactsOfType[T], A]

  /**
    * A top-level expression that handles facts of any type and returns the filtered list of facts
    * without restriction on type.
    */
  final type TerminalFactsExp = Exp[Facts, ResultSet]

  /**
    * An expression that is unbounded in its input but always returns a [[TypedResultSet]] and thus can
    * terminate a fact filter expression.
    */
  final type TypedTerminalExp[T, V] = Exp[V, TypedResultSet[T]]

  /**
    * An expression that returns a [[TypedResultSet]] with metadata and all the facts used to compute the result.
    *
    * @see [[TypedTerminalExp]] for more details
    */
  final type TypedTerminalFactsExp[T] = FactsExp[T, TypedResultSet[T]]
}
