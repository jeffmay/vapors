package com.rallyhealth.vapors.core

import cats.data.NonEmptyList
import cats.free.FreeApplicative

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{typeOf, TypeTag}

object dsl {
  import algebra._
  import data._

  /**
    * An alias to this [[dsl]] object, so you can use infix operators and more easily explore
    * the list of supported expression builders.
    */
  final val __ = this

  // TODO: Make these things more generic to things that are not Facts using typeclasses

  /**
    * The root of all expression types.
    *
    * @tparam X the type of input
    * @tparam A the free parameter, used to describe the eventual output of the [[FreeApplicative]] functor
    */
  type AnyExp[X, A] = FreeApplicative[ExpAlg[X, *], A]

  /**
    * An expression defined over all the given facts, regardless of type, that does not necessarily
    * terminate into a [[ResultSet]].
    */
  type AnyFactExp[A] = FactsExp[Any, A]

  /**
    * An expression that terminates into a boolean, used for making a conditional query or filter.
    *
    * Useful for defining sub-expressions to [[whereAnyFactHas]], [[whereAllFactsHave]], [[exists]], [[all]], etc.
    */
  type CondExp[X] = AnyExp[X, Boolean]

  /**
    * A useful alias for building a [[NamedLens]] by passing the identity lens as a starting point to a function.
    */
  type FactLensId[T] = NamedLens.Id[Fact[T]]

  /**
    * A [[NamedLens]] defined over a [[Fact]].
    */
  type FactLens[T, V] = NamedLens[Fact[T], V]

  /**
    * Alias for a non-empty list of facts of a certain type.
    */
  type Facts[T] = NonEmptyList[Fact[T]]

  /**
    * An expression that operates on a non-empty list of facts.
    *
    * @tparam T the type of fact
    * @tparam A the free parameter
    */
  type FactsExp[T, A] = AnyExp[Facts[T], A]

  /**
    * An expression that returns a [[ResultSet]] so that it can be evaluated by
    * the [[evaluator.evalQuery]] method.
    */
  type TerminalFactExp[T] = FactsExp[T, ResultSet[T]]

  /** @see [[TerminalFactExp]] */
  private def liftTerminal[T](value: ExpAlg[Facts[T], ResultSet[T]]): TerminalFactExp[T] = FreeApplicative.lift(value)

  /** @see [[FactsExp]] */
  private def liftFactsExp[T, A](value: ExpAlg[Facts[T], A]): FactsExp[T, A] = FreeApplicative.lift(value)

  /** @see [[CondExp]] */
  private def liftPredExp[X](value: ExpAlg[X, Boolean]): CondExp[X] = FreeApplicative.lift(value)

  /** @see [[AnyExp]] */
  private def liftAnyExp[X, A](value: ExpAlg[X, A]): AnyExp[X, A] = FreeApplicative.lift(value)

  // Helper methods for building conditional expressions that terminate in a functor to boolean
  private def True[X]: X => Boolean = _ => true
  private def False[X]: X => Boolean = _ => false

  // Helper methods for building terminal fact expressions that terminate in a functor to ResultSet
  private def Matched[T]: Facts[T] => ResultSet[T] = FactsMatch(_)
  private def Empty[T]: Facts[T] => ResultSet[T] = _ => NoFactsMatch()

  // TODO: Move Query stuff to another file

  /**
    * New type wrapper for a [[TerminalFactExp]] for a given type.
    *
    * @note you should call [[query]], [[queryAny]], or [[queryOf]] to get one.
    */
  // TODO: Should this even be parameterized? Operating over List[Fact[Any]] might be worth committing to
  final case class Query[T](expression: TerminalFactExp[T])

  // TODO: This should probably become the new norm and just implement a better filter / compiler error messages
  def queryAny(exp: TerminalFactExp[Any]): Query[Any] = queryOf(exp)

  /**
    * Builds a query that can handle any type of input by filtering the facts down the type expected by
    * the query.
    *
    * This is helpful for building queries with the help of type inference.
    *
    * @note if you are using appropriate [[withFactsOfTypeIn]] filters, then this will skip
    *       the step of filtering the facts to the expected type to avoid wasted computation.
    *
    * @param exp the query expression
    * @tparam U the expected type of facts for the query (if you need a specific type)
    * @return a [[Query]] that can handle any input
    */
  @deprecated(
    "Use queryAny or queryOf instead. This filters to the expected types, but is different than the way FactType selection works and could cause confusion",
    "0.0.1",
  )
  // TODO: This is too useful to remove without having the unified condition builder
  def query[U : ClassTag : TypeTag](exp: TerminalFactExp[U]): Query[Any] = {
    val uType = typeOf[U]
    if (uType =:= typeOf[Any]) {
      Query(exp.asInstanceOf[TerminalFactExp[Any]])
    } else {
      Query(
        liftFactsExp(
          ExpCollect[Facts[Any], Facts[U], ResultSet[Any]](
            uType.toString,
            facts =>
              NonEmptyList.fromList(
                facts.collect(Function.unlift {
                  case f @ Fact(typeInfo, _: U) if typeInfo.tt.tpe <:< typeOf[U] => Some(f.asInstanceOf[Fact[U]])
                  case _ => None
                }),
              ),
            exp.map(res => res: ResultSet[Any]),
            _ => NoFactsMatch(),
          ),
        ),
      )
    }
  }

  def queryOf[U](exp: TerminalFactExp[U]): Query[U] = Query(exp)

  def hasValue[T](expected: T): TerminalFactExp[T] = liftFactsExp {
    ExpFunctor[Facts[T], ResultSet[T]](facts => ResultSet.fromList(facts.filter(_.value == expected)))
  }

  @deprecated("Convert this into serializable expressions over facts.", "0.0.1")
  def filter[T](predicate: Fact[T] => Boolean): TerminalFactExp[T] = liftFactsExp {
    ExpFunctor[Facts[T], ResultSet[T]](facts => ResultSet.fromList(facts.filter(predicate)))
  }

  def whereAnyFactHas[T](exp: AnyExp[Fact[T], Boolean]): TerminalFactExp[T] = liftTerminal {
    ExpExists[Facts[T], Fact[T], ResultSet[T]](_.toList, exp, Matched, Empty)
  }

  def whereAllFactsHave[T](exp: AnyExp[Fact[T], Boolean]): TerminalFactExp[T] = liftTerminal {
    ExpForAll[Facts[T], Fact[T], ResultSet[T]](_.toList, exp, Matched, Empty)
  }

  def factTypeOf[T >: U, U : ClassTag : TypeTag](factType: FactType[U]): CondExpBuilder[T, U] = {
    factTypeIn[T, U](FactTypeSet.of(factType))
  }

  def factTypeIn[T >: U, U : ClassTag : TypeTag](factTypeSet: FactTypeSet[U]): CondExpBuilder[T, U] = {
    new CondExpBuilder[T, U]({ exp: CondExp[Fact[U]] =>
      liftAnyExp {
        ExpCollect[Fact[T], Fact[U], Boolean](
          typeOf[U].toString,
          fact => factTypeSet.matchAs(fact),
          exp,
          _ => false,
        )
      }
    })
  }

  def factValue[T](exp: CondExp[T]): CondExp[Fact[T]] = liftAnyExp {
    ExpSelectField[Fact[T], T, Boolean](Fact.value, exp)
  }

  // TODO: Use some cats typeclass instead of iterable?
  def all[F[x] <: IterableOnce[x], T, A](cond: CondExp[T]): CondExp[F[T]] = liftPredExp {
    ExpForAll[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  // TODO: Use some cats typeclass instead of iterable?
  def exists[F[x] <: IterableOnce[x], T, A](cond: CondExp[T]): CondExp[F[T]] = liftPredExp {
    ExpExists[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  def lessThan[T : Ordering](upperBound: T): CondExp[T] = liftPredExp {
    ExpWithin[T, Boolean](Window.lessThan(upperBound), True, False)
  }

  def lessThanOrEqual[T : Ordering](upperBound: T): CondExp[T] = liftPredExp {
    ExpWithin[T, Boolean](Window.lessThanOrEqual(upperBound), True, False)
  }

  def greaterThan[T : Ordering](lowerBound: T): CondExp[T] = liftPredExp {
    ExpWithin[T, Boolean](Window.greaterThan(lowerBound), True, False)
  }

  def greaterThanOrEqual[T : Ordering](lowerBound: T): CondExp[T] = liftPredExp {
    ExpWithin[T, Boolean](Window.greaterThanOrEqual(lowerBound), True, False)
  }

  def >[T : Ordering](lowerBound: T): CondExp[T] = greaterThan(lowerBound)
  def >=[T : Ordering](lowerBound: T): CondExp[T] = greaterThanOrEqual(lowerBound)
  def <[T : Ordering](upperBound: T): CondExp[T] = lessThan(upperBound)
  def <=[T : Ordering](upperBound: T): CondExp[T] = lessThanOrEqual(upperBound)

  // TODO: Figure out how to use this conditional tertiary expression

  def when[T, A](exp: AnyExp[T, Boolean])(thenExp: AnyExp[T, A])(elseExp: AnyExp[T, A]): AnyExp[T, A] = liftAnyExp {
    ExpCond[T, A](exp, thenExp, elseExp)
  }

  def withFactsOfType[T >: U, U : ClassTag : TypeTag](
    factType: FactType[U],
  )(
    exp: FactsExp[U, ResultSet[T]],
  ): TerminalFactExp[Any] = {
    withFactsOfTypeIn[T, U](FactTypeSet.of(factType))(exp)
  }

  def withFactsOfTypeIn[T >: U, U : ClassTag : TypeTag](
    factTypeSet: FactTypeSet[U],
  )(
    exp: FactsExp[U, ResultSet[T]],
  ): TerminalFactExp[Any] = liftFactsExp {
    // TODO: Figure out if there any disadvantages to using Any here
    //       If there are, we need to figure out why we get compiler errors when using type T
    ExpCollect[Facts[Any], Facts[U], ResultSet[Any]](
      typeOf[U].toString,
      facts => NonEmptyList.fromList(facts.collect(factTypeSet.matchAsPartial[U])),
      exp.map(identity),
      _ => NoFactsMatch(),
    )
  }

  // TODO: Figure out how to expand this to non-terminal expressions
  def and[T](expressions: FactsExp[T, ResultSet[T]]*): TerminalFactExp[T] = liftFactsExp {
    ExpAnd[Facts[T], ResultSet[T]](
      _.reduceLeft[ResultSet[T]]({
        case (NoFactsMatch(), _) | (_, NoFactsMatch()) => NoFactsMatch() // skip the all expressions if the first failed
        case (acc: FactsMatch[T], nextResult: FactsMatch[T]) => acc ++ nextResult // combine all the required facts
      }),
      expressions.toList,
    )
  }

  // TODO: Figure out how to expand this to non-terminal expressions
  def or[T](expressions: FactsExp[T, ResultSet[T]]*): TerminalFactExp[T] = liftFactsExp {
    ExpOr[Facts[T], ResultSet[T]](
      _.reduceLeft[ResultSet[T]]({
        case (NoFactsMatch(), b) => b // try the next expression if the first failed
        case (a @ FactsMatch(_), _) => a // take the first required set of facts
      }),
      expressions.toList,
    )
  }

  /**
    * A conditional expression builder that captures the type parameters of the expected input and output,
    * but allows freedom in sub-selecting fields without messing up type inference.
    */
  // TODO: Unify the notion of a conditional expression on a single fact with the condition-like syntax
  //       of operating over lists of facts using filters.
  final class CondExpBuilder[T, U] private[dsl] (condBuilder: CondExp[Fact[U]] => CondExp[Fact[T]]) {

    /**
      * Create a conditional expression for a fact from a field on the [[Fact]] itself.
      *
      * @note if you plan to create a lens into the value (or compare the value directly), you might want to use
      *        the [[whereValueAt]] (or [[whereValue]]) method instead.
      */
    def where(exp: CondExp[Fact[U]]): CondExp[Fact[T]] = condBuilder(exp)

    /**
      * Create a conditional expression for a fact based on the given conditional expression over the [[Fact.value]].
      */
    def whereValue(exp: CondExp[U]): CondExp[Fact[T]] = whereValueAt(identity[NamedLens.Id[U]])(exp)

    /**
      * Create a conditional expression for a fact based on the given conditional expression over any [[NamedLens]]
      * that starts from the [[Fact.value]].
      */
    def whereValueAt[V](lens: NamedLens.Id[U] => NamedLens[U, V])(exp: CondExp[V]): CondExp[Fact[T]] = {
      val namedLens = lens(NamedLens.id[U])
      condBuilder(liftAnyExp {
        ExpSelectField[Fact[U], V, Boolean](Fact.value[U].andThen(namedLens), exp)
      })
    }
  }
}
