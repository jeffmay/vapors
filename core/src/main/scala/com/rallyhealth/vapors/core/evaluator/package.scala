package com.rallyhealth.vapors.core

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.data.{Fact, ResultSet}

package object evaluator {
  import cats.instances.function._
  import dsl._

  def eval[T, A](facts: Seq[Fact[T]])(exp: FactsExp[T, A]): Option[A] = {
    NonEmptyList.fromList(facts.toList).map { matchingFacts =>
      exp.foldMap(new EvalLoop[NonEmptyList[Fact[T]]]).apply(matchingFacts)
    }
  }

  def evalQuery[R](facts: Seq[Fact[R]])(query: Query[R]): Option[ResultSet[R]] = {
    eval(facts)(query.expression)
  }
}
