package com.rallyhealth.vapors.core.dsl.factfilter

import cats.data.NonEmptyList
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg
import com.rallyhealth.vapors.core.data.{Fact, NamedLens, ResultSet}

private[dsl] trait Types {

  /**
    * The root of all expression types.
    *
    * @tparam X the type of input
    * @tparam A the free parameter, used to describe the eventual output of the [[FreeApplicative]] functor
    */
  final type AnyExp[X, A] = FreeApplicative[ExpAlg[X, *], A]

  /**
    * An expression defined over all the given facts, regardless of type, that does not necessarily
    * terminate into a [[ResultSet]].
    */
  final type AnyFactExp[A] = FactsExp[Any, A]

  /**
    * An expression that terminates into a boolean, used for making a conditional query or filter.
    *
    * Useful for defining sub-expressions to [[whereAnyFactHas]], [[whereAllFactsHave]], [[exists]], [[all]], etc.
    */
  final type CondExp[X] = AnyExp[X, Boolean]

  /**
    * A useful alias for building a [[NamedLens]] by passing the identity lens as a starting point to a function.
    */
  final type FactLensId[T] = NamedLens.Id[Fact[T]]

  /**
    * A [[NamedLens]] defined over a [[Fact]] of a known type.
    */
  final type FactLens[T, V] = NamedLens[Fact[T], V]

  /**
    * Alias for the input to a fact filter expression function.
    */
  final type Facts[T] = NonEmptyList[Fact[T]]

  /**
    * An expression that operates on a non-empty list of facts but is unbounded in what it returns.
    *
    * @tparam T the type of fact
    * @tparam A the free parameter
    */
  final type FactsExp[T, A] = AnyExp[Facts[T], A]

  /**
    * An expression that is unbounded in its input but always returns a [[ResultSet]] and thus can
    * terminate a fact filter expression.
    */
  final type TerminalExp[T, V] = AnyExp[V, ResultSet[T]]

  /**
    * An expression that returns a [[ResultSet]] with metadata and all the facts used to compute the result.
    *
    * @see [[TerminalExp]] for more details
    */
  final type TerminalFactsExp[T] = FactsExp[T, ResultSet[T]]
}
