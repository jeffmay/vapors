package com.rallyhealth

package vapors.interpreter

import vapors.data.{Evidence, ExtractBoolean, ExtractValue}
import vapors.logic.{Conjunction, Disjunction, Negation}

import cats.{Monoid, Semigroup}

/**
  * The result of a computation without any debugging information (i.e. captured parameter or result nodes)
  *
  * @param value the result of the computation
  * @param evidence the facts used in the computation
  */
final case class ExprOutput[R](
  value: R,
  evidence: Evidence,
)

object ExprOutput extends LowPriorityExprOutputInstances {

  /**
    * Logical Conjunction (aka AND)
    *
    * - If either side is false, the result is false
    * - Evidence of falseness is accumulated
    * - Evidence of truthiness requires evidence of truth on both sides, otherwise no evidence of truth
    *
    * Examples:
    *
    * | X is True | Evidence of X | Y is True | Evidence of Y | Result is True | Evidence of Result |
    * | --------- | ------------- | --------- | ------------- | -------------- | ------------------ |
    * | T         | {}            | T         | {}            | T              | {}                 |
    * | T         | {}            | F         | {}            | F              | {}                 |
    * | T         | {}            | T         | {A}           | T              | {}                 |
    * | T         | {}            | F         | {A}           | F              | {A}                |
    * | T         | {B}           | T         | {A}           | T              | {A, B}             |
    * | T         | {B}           | F         | {}            | F              | {}                 |
    * | T         | {B}           | T         | {}            | T              | {}                 |
    * | T         | {B}           | F         | {A}           | F              | {A}                |
    * | F         | {}            | T         | {A}           | F              | {}                 |
    * | F         | {B}           | F         | {A}           | F              | {A, B}             |
    * | F         | {B}           | T         | {A}           | F              | {B}                |
    * | F         | {B}           | F         | {}            | F              | {B}                |
    * | F         | {}            | F         | {A}           | F              | {}                 |
    */
  implicit def conjunction[R : Conjunction : ExtractBoolean]: Conjunction[ExprOutput[R]] =
    (lhs: ExprOutput[R], rhs: ExprOutput[R]) => {
      import cats.syntax.apply._
      @inline def isTrue(output: ExprOutput[R]): Boolean = ExtractValue[Boolean](output.value)
      val value = Conjunction[R].conjunction(lhs.value, rhs.value)
      val evidence = {
        if (ExtractValue[Boolean](value)) {
          // only combine evidence of truthiness if both sides are true
          val evTrueL = Option.when(isTrue(lhs))(lhs.evidence)
          val evTrueR = Option.when(isTrue(rhs))(rhs.evidence)
          (evTrueL, evTrueR).mapN(_ ++ _).getOrElse(Evidence.none)
        } else {
          val evFalseL = Option.unless(isTrue(lhs))(lhs.evidence)
          val evFalseR = Option.unless(isTrue(rhs))(rhs.evidence)
          // combine any evidence of falseness
          (evFalseL ++ evFalseR).foldLeft(Evidence.none) { case (l, r) => l ++ r }
        }
      }
      ExprOutput(value, evidence)
    }

  /**
    * Logical Disjunction (aka inclusive OR)
    *
    * - If either side is true, the result is true
    * - Evidence of truthiness is accumulated
    * - Evidence of falseness requires evidence of false on both sides, otherwise there is no evidence of false
    *
    * Examples:
    *
    * | X is True | Evidence of X | Y is True | Evidence of Y | Result is True | Evidence of Result |
    * | --------- | ------------- | --------- | ------------- | -------------- | ------------------ |
    * | T         | {}            | T         | {}            | T              | {}                 |
    * | T         | {}            | F         | {}            | T              | {}                 |
    * | T         | {}            | T         | {A}           | T              | {A}                |
    * | T         | {}            | F         | {A}           | T              | {}                 |
    * | T         | {B}           | T         | {A}           | T              | {A, B}             |
    * | T         | {B}           | F         | {}            | T              | {B}                |
    * | T         | {B}           | T         | {}            | T              | {B}                |
    * | T         | {B}           | F         | {A}           | T              | {B}                |
    * | F         | {}            | T         | {A}           | T              | {A}                |
    * | F         | {B}           | F         | {A}           | F              | {A, B}             |
    * | F         | {B}           | T         | {A}           | T              | {A}                |
    * | F         | {B}           | F         | {}            | F              | {}                 |
    * | F         | {}            | F         | {A}           | F              | {}                 |
    */
  implicit def disjunction[R : Disjunction : ExtractBoolean]: Disjunction[ExprOutput[R]] =
    (lhs: ExprOutput[R], rhs: ExprOutput[R]) => {
      import cats.syntax.apply._
      @inline def isTrue(output: ExprOutput[R]): Boolean = ExtractValue[Boolean](output.value)
      val value = Disjunction[R].disjunction(lhs.value, rhs.value)
      val evidence = {
        if (ExtractValue[Boolean](value)) {
          // combine all evidence of truthiness from sides that are truthy
          val evTrueL = Option.when(isTrue(lhs))(lhs.evidence)
          val evTrueR = Option.when(isTrue(rhs))(rhs.evidence)
          (evTrueL ++ evTrueR).foldLeft(Evidence.none) { case (l, r) => l ++ r }
        } else {
          // only combine evidence of falseness if both sides are false
          val evFalseL = Option.unless(lhs.evidence.isEmpty)(lhs.evidence)
          val evFalseR = Option.unless(rhs.evidence.isEmpty)(rhs.evidence)
          (evFalseL, evFalseR).mapN(_ ++ _).getOrElse(Evidence.none)
        }
      }
      ExprOutput(value, evidence)
    }

  /**
    * Negates the result value without affecting the [[Evidence]].
    */
  implicit def negation[R : Negation]: Negation[ExprOutput[R]] = { output =>
    val negatedValue = Negation[R].negation(output.value)
    output.copy(value = negatedValue)
  }

  /**
    * @see [[semigroup]] for more details. Adds definition for empty.
    */
  implicit def monoid[A : Monoid]: Monoid[ExprOutput[A]] = new MonoidExprOutput[A]

  private class MonoidExprOutput[A : Monoid] extends SemigroupExprOutput[A] with Monoid[ExprOutput[A]] {
    override final def empty: ExprOutput[A] =
      ExprOutput(Monoid[A].empty, Evidence.none)
  }
}

sealed class LowPriorityExprOutputInstances {

  /**
    * Combine two monoidal values and union their evidence.
    *
    * @note This is not _always_ safe. There may be some combinations of values in which you must combine
    *       [[Evidence]] for the resulting value differently based on the inputs. However, this is not the
    *       general purpose or intent of saying something is a [[Monoid]], so this definition should be
    *       generally safe. For example, there is no standard definition of [[Monoid]] for Boolean, because
    *       there is no safe assumption for and "empty" boolean value. However, evidence of true || true is
    *       not necessarily the same as evidence of true && true. Any definitions for which this distinction
    *       matters will typically use its own typeclasses, such as [[Conjunction]] or [[Disjunction]].
    */
  implicit def semigroup[A : Semigroup]: Semigroup[ExprOutput[A]] = new SemigroupExprOutput[A]

  protected class SemigroupExprOutput[A : Semigroup] extends Semigroup[ExprOutput[A]] {
    override final def combine(
      x: ExprOutput[A],
      y: ExprOutput[A],
    ): ExprOutput[A] = {
      ExprOutput(
        Semigroup[A].combine(x.value, y.value),
        x.evidence ++ y.evidence,
      )
    }
  }
}
