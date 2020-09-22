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

  type TerminalExp[T, V] = AnyExp[V, ResultSet[T]]

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
  type TerminalFactsExp[T] = FactsExp[T, ResultSet[T]]

  /** @see [[TerminalFactsExp]] */
  private def liftTermExp[T](value: ExpAlg[Facts[T], ResultSet[T]]): TerminalFactsExp[T] = FreeApplicative.lift(value)

  /** @see [[FactsExp]] */
  private def liftFactsExp[T, A](value: ExpAlg[Facts[T], A]): FactsExp[T, A] = FreeApplicative.lift(value)

  /** @see [[CondExp]] */
  private def liftCondExp[X](value: ExpAlg[X, Boolean]): CondExp[X] = FreeApplicative.lift(value)

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
    * New type wrapper for a [[TerminalFactsExp]] for a given type.
    *
    * @note you should call [[query]], [[queryAny]], or [[queryOf]] to get one.
    */
  // TODO: Should this even be parameterized? Operating over List[Fact[Any]] might be worth committing to
  final case class Query[T](expression: TerminalFactsExp[T])

  // TODO: This should probably become the new norm and just implement a better filter / compiler error messages
  def queryAny(exp: TerminalFactsExp[Any]): Query[Any] = queryOf(exp)

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
  def query[U : ClassTag : TypeTag](exp: TerminalFactsExp[U]): Query[Any] = {
    val uType = typeOf[U]
    if (uType =:= typeOf[Any]) {
      Query(exp.asInstanceOf[TerminalFactsExp[Any]])
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

  def queryOf[U](exp: TerminalFactsExp[U]): Query[U] = Query(exp)

  @deprecated("Select the fact type first before comparing equality.", "0.0.1")
  def hasValue[T](expected: T): TerminalFactsExp[T] = liftFactsExp {
    ExpFunctor[Facts[T], ResultSet[T]](facts => ResultSet.fromList(facts.filter(_.value == expected)))
  }

  @deprecated("Convert this into serializable expressions over facts.", "0.0.1")
  def filter[T](predicate: Fact[T] => Boolean): TerminalFactsExp[T] = liftFactsExp {
    ExpFunctor[Facts[T], ResultSet[T]](facts => ResultSet.fromList(facts.filter(predicate)))
  }

  // TODO: Use some cats typeclass instead of iterable?
  def all[F[x] <: IterableOnce[x], T, A](cond: CondExp[T]): CondExp[F[T]] = liftCondExp {
    ExpForAll[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  // TODO: Use some cats typeclass instead of iterable?
  def exists[F[x] <: IterableOnce[x], T, A](cond: CondExp[T]): CondExp[F[T]] = liftCondExp {
    ExpExists[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  def lessThan[T : Ordering](upperBound: T): CondExp[T] = liftCondExp {
    ExpWithin[T, Boolean](Window.lessThan(upperBound), True, False)
  }

  def lessThanOrEqual[T : Ordering](upperBound: T): CondExp[T] = liftCondExp {
    ExpWithin[T, Boolean](Window.lessThanOrEqual(upperBound), True, False)
  }

  def greaterThan[T : Ordering](lowerBound: T): CondExp[T] = liftCondExp {
    ExpWithin[T, Boolean](Window.greaterThan(lowerBound), True, False)
  }

  def greaterThanOrEqual[T : Ordering](lowerBound: T): CondExp[T] = liftCondExp {
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

  def withFactsOfType[U : ClassTag : TypeTag](factType: FactType[U]): TypedFactExpBuilder[Any, U] = {
    withFactsOfTypeIn[Any, U](FactTypeSet.of(factType))
  }

  def withFactsOfTypeIn[T >: U, U : ClassTag : TypeTag](factTypeSet: FactTypeSet[U]): TypedFactExpBuilder[T, U] = {
    new TypedFactExpBuilder[T, U](factTypeSet)
  }

  def and[T, A : Intersect](
    one: AnyExp[T, A],
    two: AnyExp[T, A],
    others: AnyExp[T, A]*,
  ): AnyExp[T, A] = liftAnyExp {
    ExpAnd[T, A](
      Intersect[A].intersect,
      one :: two :: others.toList,
    )
  }

  def or[T, A : Union](
    one: AnyExp[T, A],
    two: AnyExp[T, A],
    others: AnyExp[T, A]*,
  ): AnyExp[T, A] = liftAnyExp {
    ExpOr[T, A](
      Union[A].union,
      one :: two :: others.toList,
    )
  }

  def typeNameOf[T : TypeTag]: String = typeOf[T].toString.split('.').dropWhile(_.charAt(0).isLower).mkString(".")

  final class FactValuesExpBuilder[T >: U, U : ClassTag : TypeTag, V] private[dsl] (
    factTypeSet: FactTypeSet[U],
    factLens: NamedLens[Fact[U], V],
  ) {

    private def selectFactValuesWhere(exp: ExpAlg[Facts[U], ResultSet[T]]): TerminalFactsExp[T] = {
      liftTermExp {
        ExpCollect[Facts[T], Facts[U], ResultSet[T]](
          s"Fact[${typeNameOf[U]}]",
          facts => NonEmptyList.fromList(facts.collect(factTypeSet.matchAsPartial[U])),
          liftFactsExp(exp),
          Empty,
        )
      }
    }

    def whereAllValues(condExp: CondExp[V]): TerminalFactsExp[T] = {
      selectFactValuesWhere {
        ExpForAll[Facts[U], Fact[U], ResultSet[T]](
          _.toList,
          liftCondExp {
            ExpSelectField[Fact[U], V, Boolean](factLens, condExp)
          },
          Matched,
          Empty,
        )
      }
    }

    def whereAnyValue(condExp: CondExp[V]): TerminalFactsExp[T] = {
      selectFactValuesWhere {
        ExpExists[Facts[U], Fact[U], ResultSet[T]](
          _.toList,
          liftCondExp {
            ExpSelectField[Fact[U], V, Boolean](factLens, condExp)
          },
          Matched,
          Empty,
        )
      }
    }
  }

  final class TypedFactExpBuilder[T >: U, U : ClassTag : TypeTag] private[dsl] (factTypeSet: FactTypeSet[U]) {

    // TODO: These terminal expression methods can be optimized to avoid an unnecessary select expression

    def whereAnyFact(condExp: CondExp[Fact[U]]): TerminalFactsExp[T] = {
      new FactValuesExpBuilder[T, U, Fact[U]](factTypeSet, NamedLens.id[Fact[U]]).whereAnyValue(condExp)
    }

    def whereAnyValue(condExp: CondExp[U]): TerminalFactsExp[T] = {
      new FactValuesExpBuilder[T, U, U](factTypeSet, Fact.value[U]).whereAnyValue(condExp)
    }

    def whereAllFacts(condExp: CondExp[Fact[U]]): TerminalFactsExp[T] = {
      new FactValuesExpBuilder[T, U, Fact[U]](factTypeSet, NamedLens.id[Fact[U]]).whereAllValues(condExp)
    }

    def whereAllValues(condExp: CondExp[U]): TerminalFactsExp[T] = {
      new FactValuesExpBuilder[T, U, U](factTypeSet, Fact.value[U]).whereAllValues(condExp)
    }

    def withValuesAt[V](lens: NamedLens.Id[U] => NamedLens[U, V]): FactValuesExpBuilder[T, U, V] = {
      new FactValuesExpBuilder(factTypeSet, Fact.value[U].andThen(lens(NamedLens.id[U])))
    }
  }

}
