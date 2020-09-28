package com.rallyhealth.vapors.core.dsl.factfilter

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.logic.{Intersect, Union}
import com.rallyhealth.vapors.core.{algebra, data, dsl}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{typeOf, TypeTag}

private[dsl] class Dsl extends Types with Syntax {

  import algebra._
  import data._

  // TODO: Use some cats typeclass instead of iterable?
  def all[F[x] <: IterableOnce[x], T, A](cond: CondExp[T]): CondExp[F[T]] = liftCondExp {
    ExpAlg.ForAll[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  // TODO: Use some cats typeclass instead of iterable?
  def exists[F[x] <: IterableOnce[x], T, A](cond: CondExp[T]): CondExp[F[T]] = liftCondExp {
    ExpAlg.Exists[F[T], T, Boolean](identity[F[T]], cond, True, False)
  }

  def lessThan[T : Ordering](upperBound: T): CondExp[T] = liftCondExp {
    ExpAlg.Within[T, Boolean](Window.lessThan(upperBound), True, False)
  }

  def lessThanOrEqual[T : Ordering](upperBound: T): CondExp[T] = liftCondExp {
    ExpAlg.Within[T, Boolean](Window.lessThanOrEqual(upperBound), True, False)
  }

  def greaterThan[T : Ordering](lowerBound: T): CondExp[T] = liftCondExp {
    ExpAlg.Within[T, Boolean](Window.greaterThan(lowerBound), True, False)
  }

  def greaterThanOrEqual[T : Ordering](lowerBound: T): CondExp[T] = liftCondExp {
    ExpAlg.Within[T, Boolean](Window.greaterThanOrEqual(lowerBound), True, False)
  }

  // TODO: Figure out how to use this conditional tertiary expression

  def when[T, A](exp: AnyExp[T, Boolean])(thenExp: AnyExp[T, A])(elseExp: AnyExp[T, A]): AnyExp[T, A] = liftAnyExp {
    ExpAlg.Cond[T, A](exp, thenExp, elseExp)
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
    ExpAlg.And[T, A](
      Intersect[A].intersect,
      one :: two :: others.toList,
    )
  }

  def or[T, A : Union](
    one: AnyExp[T, A],
    two: AnyExp[T, A],
    others: AnyExp[T, A]*,
  ): AnyExp[T, A] = liftAnyExp {
    ExpAlg.Or[T, A](
      Union[A].union,
      one :: two :: others.toList,
    )
  }

  def typeNameOf[T : TypeTag]: String = typeOf[T].toString.split('.').dropWhile(_.charAt(0).isLower).mkString(".")

  def alwaysTrue[T]: CondExp[T] = liftCondExp(ExpAlg.Pure("True", _ => true))

  def alwaysFalse[T]: CondExp[T] = liftCondExp(ExpAlg.Pure("False", _ => false))

  def alwaysMatch: TerminalFactsExp[Any] = liftTermExp(ExpAlg.Pure("AlwaysMatch", FactsMatch(_)))

  def alwaysEmpty: TerminalFactsExp[Any] = liftTermExp(ExpAlg.Pure("AlwaysEmpty", _ => NoFactsMatch()))

  // Helper methods for building conditional expressions that terminate in a functor to boolean
  private def True[X]: X => Boolean = _ => true
  private def False[X]: X => Boolean = _ => false

  /** @see [[TerminalFactsExp]] */
  private def liftTermExp[T](value: ExpAlg[Facts[T], ResultSet[T]]): TerminalFactsExp[T] = FreeApplicative.lift(value)

  /** @see [[CondExp]] */
  private def liftCondExp[X](value: ExpAlg[X, Boolean]): CondExp[X] = FreeApplicative.lift(value)

  /** @see [[AnyExp]] */
  private def liftAnyExp[X, A](value: ExpAlg[X, A]): AnyExp[X, A] = FreeApplicative.lift(value)
}
