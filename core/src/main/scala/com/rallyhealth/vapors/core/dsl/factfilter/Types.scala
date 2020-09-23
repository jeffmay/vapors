package com.rallyhealth.vapors.core.dsl.factfilter

import cats.data.NonEmptyList
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg
import com.rallyhealth.vapors.core.data.{Fact, NamedLens, ResultSet, TypedFact, TypedResultSet}

private[dsl] trait Types {

  /**
    * Alias for a collection of facts with no restriction on Scala type.
    *
    * @note This is the input for a top-level fact filter expression.
    */
  final type Facts = NonEmptyList[Fact]

  /**
    * Alias for a collection of facts of a specific type.
    */
  final type FactsOfType[T] = NonEmptyList[TypedFact[T]]

  /**
    * A [[NamedLens]] defined over a [[TypedFact]] of a known type.
    */
  final type FactLens[T, V] = NamedLens[TypedFact[T], V]

  /**
    * A useful alias for building a [[NamedLens]] by passing the identity lens as a starting point to a function.
    */
  final type FactLensId[T] = NamedLens.Id[TypedFact[T]]

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
    * Useful for defining sub-expressions to [[whereAnyFactHas]], [[whereAllFactsHave]], [[exists]], [[all]], etc.
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
