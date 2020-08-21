package com.rallyhealth.vapors.core

import cats.data.NonEmptyList
import cats.free.FreeApplicative
import cats.instances.function._
import cats.~>
import com.rallyhealth.vapors.core.data.Fact

object evaluator {
  import algebra._
  import dsl._

  type EvalF[T, A] = T => A

  type EvalExp[T, A] = FreeApplicative[ExpAlg[T, *], A]

  private final class EvalLoop[T] extends (ExpAlg[T, *] ~> EvalF[T, *]) {
    override def apply[A](fa: ExpAlg[T, A]): EvalF[T, A] = evalF(fa, _)

    private def evalF[B](
      exp: ExpAlg[T, B],
      data: T
    ): B = exp match {
      case ExpFunctor(map) =>
        map(data)
      case ExpSelectField(_, selector, expression) =>
        evalLoop(expression)(selector(data))
      case ExpExists(toIterable, condition, whenTrue, whenFalse) =>
        // we don't need this intermediate list and can remove it for performance,
        // but for the time being this is a good place to put a debugger
        val results = toIterable(data).iterator.map(evalLoop(condition)).toList
        val success = results.exists(identity[Boolean])
        if (success) whenTrue(data)
        else whenFalse(data)
      case ExpForAll(toIterable, condition, whenTrue, whenFalse) =>
        // we don't need this intermediate list and can remove it for performance,
        // but for the time being this is a good place to put a debugger
        val results = toIterable(data).iterator.map(evalLoop(condition)).toList
        val success = results.forall(identity[Boolean])
        if (success) whenTrue(data)
        else whenFalse(data)
      case ExpWithin(window, whenTrue, whenFalse) =>
        if (window.contains(data)) whenTrue(data) else whenFalse(data)
      case ExpCond(condition, thenExpression, elseExpression) =>
        val success = evalLoop(condition)(data)
        if (success) evalLoop(thenExpression)(data)
        else evalLoop(elseExpression)(data)
      case ExpCollect(_, collector, expression, whenEmpty) =>
        collector(data).map(evalLoop(expression)).getOrElse(whenEmpty(data))
      case ExpAnd(combine, expressions) =>
        combine(expressions.map(evalLoop[T, B]).map(_(data)))
      case ExpOr(combine, expressions) =>
        combine(expressions.map(evalLoop[T, B]).map(_(data)))
    }
  }

  private def evalLoop[T, A](exp: EvalExp[T, A]): EvalF[T, A] = {
    exp.foldMap(new EvalLoop[T])
  }

  def eval[T, A](facts: Seq[Fact[T]])(exp: FactExp[T, A]): Option[A] = {
    NonEmptyList.fromList(facts.toList).map { matchingFacts =>
      evalLoop(exp)(matchingFacts)
    }
  }

  def evalQuery[R](facts: Seq[Fact[R]])(query: Query[R]): Option[ExpRes[R]] = {
    eval(facts)(query.expression)
  }
}
