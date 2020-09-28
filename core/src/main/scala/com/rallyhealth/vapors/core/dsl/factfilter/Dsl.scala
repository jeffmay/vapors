package com.rallyhealth.vapors.core.dsl.factfilter

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.logic.{Intersect, Union}
import com.rallyhealth.vapors.core.{algebra, data}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

private[dsl] class Dsl extends Evaluation with Types with Syntax {

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

  def when[T, A](exp: Exp[T, Boolean])(thenExp: Exp[T, A])(elseExp: Exp[T, A]): Exp[T, A] = liftExp {
    ExpAlg.Cond[T, A](exp, thenExp, elseExp)
  }

  def withFactsOfType[T >: U, U : ClassTag : TypeTag](factType: FactType[U]): WhereFactsExpBuilder[T, U] = {
    new WhereFactsExpBuilder[T, U](FactTypeSet.of(factType))
  }

  def withFactsOfTypeIn[T >: U, U : ClassTag : TypeTag](factTypeSet: FactTypeSet[U]): WhereFactsExpBuilder[T, U] = {
    new WhereFactsExpBuilder[T, U](factTypeSet)
  }

  def and[T, A : Intersect](
    one: Exp[T, A],
    two: Exp[T, A],
    others: Exp[T, A]*,
  ): Exp[T, A] = liftExp {
    ExpAlg.And[T, A](
      Intersect[A].intersect,
      one :: two :: others.toList,
    )
  }

  def or[T, A : Union](
    one: Exp[T, A],
    two: Exp[T, A],
    others: Exp[T, A]*,
  ): Exp[T, A] = liftExp {
    ExpAlg.Or[T, A](
      Union[A].union,
      one :: two :: others.toList,
    )
  }

  def alwaysTrue[T]: CondExp[T] = liftCondExp(ExpAlg.Pure("True", _ => true))

  def alwaysFalse[T]: CondExp[T] = liftCondExp(ExpAlg.Pure("False", _ => false))

  def alwaysMatch: TerminalFactsExp = liftAnyTermExp(ExpAlg.Pure("AlwaysMatch", FactsMatch(_)))

  def alwaysEmpty: TerminalFactsExp = liftAnyTermExp(ExpAlg.Pure("AlwaysEmpty", _ => NoFactsMatch()))

  // Helper methods for building conditional expressions that terminate in a functor to boolean
  private def True[X]: X => Boolean = _ => true
  private def False[X]: X => Boolean = _ => false

  /** @see [[FactsExp]] */
  private def liftAnyTermExp(exp: ExpAlg[Facts, ResultSet]): TerminalFactsExp =
    FreeApplicative.lift(exp)

  /** @see [[CondExp]] */
  private def liftCondExp[X](exp: ExpAlg[X, Boolean]): CondExp[X] = FreeApplicative.lift(exp)

  /** @see [[Exp]] */
  private def liftExp[X, A](exp: ExpAlg[X, A]): Exp[X, A] = FreeApplicative.lift(exp)
}
