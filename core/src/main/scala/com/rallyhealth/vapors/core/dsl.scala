package com.rallyhealth.vapors.core

import cats.data.NonEmptyList
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.data._

import scala.collection.immutable.NumericRange
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{TypeTag, typeOf}

object dsl {
  import algebra._

  implicit def window(range: Range): Window[Int] = Window.fromRange(range)
  implicit def window[T : Ordering](range: NumericRange[T]): Window[T] = Window.fromRange(range)

  // TODO: Make these things more generic to things that are not Facts using typeclasses
  type AnyExp[X, A] = FreeApplicative[ExpAlg[X, *], A]
  type ExpRes[+T] = ResultSet[T]
  type AnyFactExp[A] = FactExp[Any, A]
  type Facts[T] = NonEmptyList[Fact[T]]
  type FactExp[T, A] = AnyExp[Facts[T], A]

  /**
    * An expression whose free type parameter is a [[ResultSet]] that can be evaluated by
    * the [[evaluator.evalQuery]] method.
    */
  type TerminalFactExp[T] = FactExp[T, ExpRes[T]]

  /**
    * Terminal expressions are required at the root level of a FQL query.
    */
  private def liftTerminal[T](value: ExpAlg[Facts[T], ExpRes[T]]): TerminalFactExp[T] = FreeApplicative.lift(value)

  private def liftFactExp[T, A](value: ExpAlg[Facts[T], A]): FactExp[T, A] = FreeApplicative.lift(value)

  private def liftPredExp[X](value: ExpAlg[X, Boolean]): AnyExp[X, Boolean] = FreeApplicative.lift(value)

  private def liftAnyExp[X, A](value: ExpAlg[X, A]): AnyExp[X, A] = FreeApplicative.lift(value)

  private def True[X]: X => Boolean = _ => true

  private def False[X]: X => Boolean = _ => false

  private def Matched[T]: Facts[T] => ExpRes[T] = FactsMatch(_)

  private def Empty[T]: Facts[T] => ExpRes[T] = _ => NoFactsMatch()

  /**
    * New type wrapper for a [[TerminalFactExp]] for a given type.
    *
    * @note you should call [[query]] or [[queryOf]] to get one.
    */
  final case class Query[T](expression: TerminalFactExp[T])

  /**
    * Builds a query that can handle any type of input by filtering the facts down the type expected by
    * the query.
    *
    * This is helpful for building queries with the help of type inference.
    *
    * @note if you are using appropriate [[withFactTypes]] filters, then this will skip
    *       the step of filtering the facts to the expected type to avoid wasted computation.
    *
    * @param exp the query expression
    * @tparam U the expected type of facts for the query (if you need a specific type)
    * @return a [[Query]] that can handle any input
    */
  def query[U : ClassTag : TypeTag](exp: TerminalFactExp[U]): Query[Any] = {
    val uType = typeOf[U]
    if (uType =:= typeOf[Any]) {
      Query(exp.asInstanceOf[TerminalFactExp[Any]])
    } else {
      Query(
        liftFactExp(
          ExpCollect[Facts[Any], Facts[U], ExpRes[Any]](
            uType.toString,
            facts => {
              NonEmptyList.fromList(facts.collect(Function.unlift {
                case f @ Fact(typeInfo, _: U) if typeInfo.tt.tpe <:< typeOf[U] => Some(f.asInstanceOf[Fact[U]])
                case _ => None
              }))
            },
            exp.map(res => res: ExpRes[Any]),
            _ => NoFactsMatch()
          )
        )
      )
    }
  }

  def queryOf[U](exp: TerminalFactExp[U]): Query[U] = Query(exp)

  def hasValue[T](expected: T): FactExp[T, ExpRes[T]] = liftFactExp {
    ExpFunctor[Facts[T], ExpRes[T]](facts => ResultSet.fromList(facts.filter(_.value == expected)))
  }

  def filter[T](predicate: Fact[T] => Boolean): TerminalFactExp[T] = liftFactExp {
    ExpFunctor[Facts[T], ExpRes[T]](facts => ResultSet.fromList(facts.filter(predicate)))
  }

  def anyFact[T](exp: AnyExp[Fact[T], Boolean]): TerminalFactExp[T] = liftTerminal {
    ExpExists[Facts[T], Fact[T], ExpRes[T]](_.toList, exp, Matched, Empty)
  }

  def allFacts[T](exp: AnyExp[Fact[T], Boolean]): TerminalFactExp[T] = liftTerminal {
    ExpForAll[Facts[T], Fact[T], ExpRes[T]](_.toList, exp, Matched, Empty)
  }

  // TODO: Use lens here
  def withFieldValue[T, U](
    getter: Fact[T] => U,
    fieldName: String
  )(
    exp: AnyExp[U, Boolean]
  ): AnyExp[Fact[T], Boolean] = liftAnyExp {
    ExpSelectField[Fact[T], U, Boolean](fieldName, getter, exp)
  }

  // TODO: Use some cats typeclass instead of iterable?
  def all[F[x] <: IterableOnce[x], T, A](cond: AnyExp[T, Boolean]): AnyExp[F[T], Boolean] = liftPredExp {
    ExpForAll[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  // TODO: Use some cats typeclass instead of iterable?
  def exists[F[x] <: IterableOnce[x], T, A](cond: AnyExp[T, Boolean]): AnyExp[F[T], Boolean] = liftPredExp {
    ExpExists[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  def greaterThan[T : Ordering](lowerBound: T): AnyExp[T, Boolean] = liftPredExp {
    ExpWithin[T, Boolean](Window.after(lowerBound), True, False)
  }

  def when[T, A](exp: AnyExp[T, Boolean])(thenExp: AnyExp[T, A])(elseExp: AnyExp[T, A]): AnyExp[T, A] = liftAnyExp {
    ExpCond[T, A](exp, thenExp, elseExp)
  }
  // TODO: See above

  def withFactType[T >: U, U : ClassTag : TypeTag](
    factType: FactType[U]
  )(
    exp: FactExp[U, ExpRes[T]]
  ): TerminalFactExp[Any] = {
    withFactTypes[T, U](FactTypeSet.of(factType))(exp)
  }

  def withFactTypes[T >: U, U : ClassTag : TypeTag](
    factTypeSet: FactTypeSet[U]
  )(
    exp: FactExp[U, ExpRes[T]]
  ): TerminalFactExp[Any] = liftFactExp {
    // TODO: Figure out if there any disadvantages to using Any here
    //       If there are, we need to figure out why we get compiler errors when using type T
    ExpCollect[Facts[Any], Facts[U], ExpRes[Any]](
      typeOf[U].toString,
      facts => NonEmptyList.fromList(facts.collect(factTypeSet.matchAsPartial[U])),
      exp.map(identity),
      _ => NoFactsMatch()
    )
  }

  // TODO: Figure out how to expand this to non-terminal expressions
  def and[T](expressions: NonEmptyList[FactExp[T, ExpRes[T]]]): TerminalFactExp[T] = liftFactExp {
    ExpAnd[Facts[T], ExpRes[T]](
      _.reduceLeft[ExpRes[T]]({
        case (NoFactsMatch(), _) | (_, NoFactsMatch()) => NoFactsMatch() // skip the all expressions if the first failed
        case (acc: FactsMatch[T], nextResult: FactsMatch[T]) => acc ++ nextResult // combine all the required facts
      }),
      expressions
    )
  }

  // TODO: Figure out how to expand this to non-terminal expressions
  def or[T](expressions: NonEmptyList[FactExp[T, ExpRes[T]]]): TerminalFactExp[T] = liftFactExp {
    ExpOr[Facts[T], ExpRes[T]](
      _.reduceLeft[ExpRes[T]]({
        case (NoFactsMatch(), b) => b // try the next expression if the first failed
        case (a @ FactsMatch(_), _) => a // take the first required set of facts
      }),
      expressions
    )
  }
}
