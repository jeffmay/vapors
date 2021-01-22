package com.rallyhealth.vapors.factfilter.evaluator

import cats.{Eval, FlatMap, Foldable, Functor, FunctorFilter, Traverse, TraverseFilter}
import cats.kernel.Monoid
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}
import com.rallyhealth.vapors.factfilter.data.{ExtractBoolean, FactSet}
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsResultFn.{Input, Output}
import shapeless.HList

abstract class VisitGenericExprWithProxy[F[_] : Foldable, V, P, G[_]]
  extends Expr.Visitor[F, V, P, Lambda[r => Input[F, V] => G[r]]] {

  protected def visitGeneric[M[_] : Foldable, U, R](
    expr: Expr[M, U, R, P],
    input: Input[M, U],
  ): G[R]

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitAnd[R : Conjunction : ExtractBoolean](expr: Expr.And[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[F, V, M, U, R, P],
  ): Input[F, V] => G[R] = visitGeneric(expr, _)

  override def visitConstOutput[R](expr: Expr.ConstOutput[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitDefine[M[_] : Foldable, T](
    expr: Expr.Define[M, T, P],
  ): Input[F, V] => (Output[FactSet], List[Eval[P]]) = { input =>
    visitGeneric(expr, input.withValue(input.factTable))
  }

  override def visitEmbed[R](expr: Expr.Embed[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitExistsInOutput[M[_] : Foldable, U](
    expr: Expr.ExistsInOutput[F, V, M, U, P],
  ): Input[F, V] => (Output[Boolean], List[Eval[P]]) = visitGeneric(expr, _)

  override def visitFilterOutput[M[_] : Foldable : FunctorFilter, R](
    expr: Expr.FilterOutput[F, V, M, R, P],
  ): Input[F, V] => (Output[M[R]], List[Eval[P]]) = visitGeneric(expr, _)

  override def visitFlatMapOutput[M[_] : Foldable : FlatMap, U, X](
    expr: Expr.FlatMapOutput[F, V, M, U, X, P],
  ): Input[F, V] => (Output[M[X]], List[Eval[P]]) = visitGeneric(expr, _)

  override def visitMapOutput[M[_] : Foldable : Functor, U, R](
    expr: Expr.MapOutput[F, V, M, U, R, P],
  ): Input[F, V] => (Output[M[R]], List[Eval[P]]) = visitGeneric(expr, _)

  override def visitNegativeOutput[R : Negative](expr: Expr.NegativeOutput[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitNot[R : Negation](expr: Expr.Not[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitOr[R : Disjunction : ExtractBoolean](expr: Expr.Or[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitOutputIsEmpty[M[_] : Foldable, R](
    expr: Expr.OutputIsEmpty[F, V, M, R, P],
  ): Input[F, V] => (Output[Boolean], List[Eval[P]]) = visitGeneric(expr, _)

  override def visitOutputWithinSet[R](
    expr: Expr.OutputWithinSet[F, V, R, P],
  ): Input[F, V] => (Output[Boolean], List[Eval[P]]) = visitGeneric(expr, _)

  override def visitOutputWithinWindow[R](
    expr: Expr.OutputWithinWindow[F, V, R, P],
  ): Input[F, V] => (Output[Boolean], List[Eval[P]]) = visitGeneric(expr, _)

  override def visitReturnInput(expr: Expr.ReturnInput[F, V, P]): Input[F, V] => (Output[F[V]], List[Eval[P]]) =
    visitGeneric(expr, _)

  override def visitSelectFromOutput[S, R](expr: Expr.SelectFromOutput[F, V, S, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitSubtractOutputs[R : Subtraction](expr: Expr.SubtractOutputs[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](
    expr: Expr.TakeFromOutput[F, V, M, R, P],
  ): Input[F, V] => (Output[M[R]], List[Eval[P]]) = visitGeneric(expr, _)

  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitWhen[R](expr: Expr.When[F, V, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)

  override def visitWithFactsOfType[T, R](expr: Expr.WithFactsOfType[T, R, P]): Input[F, V] => G[R] = { input =>
    visitGeneric(expr, input.withValue(input.factTable))
  }

  override def visitWrapOutput[T <: HList, R](expr: Expr.WrapOutput[F, V, T, R, P]): Input[F, V] => G[R] =
    visitGeneric(expr, _)
}
