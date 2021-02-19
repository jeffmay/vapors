package com.rallyhealth.vapors.core.dsl

import cats.data.NonEmptyList
import cats.{Foldable, Id, Monoid}
import com.rallyhealth.vapors.core.algebra.{CaptureP, ConditionBranch, Expr, ExprResult}
import com.rallyhealth.vapors.core.data._
import com.rallyhealth.vapors.core.dsl
import com.rallyhealth.vapors.core.interpreter.{ExprInput, InterpretExprAsResultFn}
import com.rallyhealth.vapors.core.lens.NamedLens
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}

object ExprDsl extends ExprDsl {

  @deprecated("Use com.rallyhealth.vapors.core.dsl.CondExpr instead.", "0.8.0")
  final type CondExpr[V, P] = Expr[V, Boolean, P]

  @deprecated("Use com.rallyhealth.vapors.core.dsl.ValExpr instead.", "0.8.0")
  final type ValExpr[V, R, P] = Expr[V, R, P]

  @deprecated("Use com.rallyhealth.vapors.core.dsl.ValCondExpr instead.", "0.8.0")
  final type ValCondExpr[V, P] = dsl.ValExpr[V, Boolean, P]

  @deprecated("Use com.rallyhealth.vapors.core.dsl.RootExpr instead.", "0.8.0")
  final type RootExpr[R, P] = Expr[FactTable, R, P]
}

// TODO: Remove methods that are not useful anymore and move less-useful methods to a separate object
trait ExprDsl extends WrapExprSyntax with WrapEachExprSyntax {

  def eval[R, P](facts: FactTable)(query: RootExpr[R, P]): ExprResult[FactTable, R, P] = {
    InterpretExprAsResultFn(query)(ExprInput.fromFactTable(facts))
  }

  /**
    * Lifts the given value into the output of an expression with no evidence.
    */
  def const[R, P](
    value: R,
    evidence: Evidence = Evidence.none,
  )(implicit
    capture: CaptureRootExpr[R, P],
  ): Expr.ConstOutput[FactTable, R, P] =
    Expr.ConstOutput(value, evidence, capture)

  /**
    * Uses the value of a given fact and also considers the fact as evidence of its own value.
    */
  def factValue[R, P](
    typedFact: TypedFact[R],
  )(implicit
    capture: CaptureRootExpr[R, P],
  ): Expr.ConstOutput[FactTable, R, P] =
    const(typedFact.value, Evidence(typedFact))

  def input[V, P](
    implicit
    capture: CaptureP[V, V, P],
  ): Expr.ReturnInput[V, P] =
    Expr.ReturnInput(capture)

  def define[T](factType: FactType[T]): DefinitionExprBuilder[T] = new DefinitionExprBuilder(factType)

  def usingDefinitions[V, R, P](
    definitions: Expr.Definition[P]*,
  )(
    subExpr: Expr[V, R, P],
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.UsingDefinitions[V, R, P] =
    Expr.UsingDefinitions(definitions.toVector, subExpr, captureResult)

  def embed[V, R, P](
    expr: RootExpr[R, P],
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.Embed[V, R, P] =
    Expr.Embed(expr, captureResult)

  def concat[V, M[_], R, P](expressions: Expr[V, M[R], P]*): ConcatOutputExprBuilder[V, M, R, P] =
    new ConcatOutputExprBuilder(expressions.to(LazyList))

  def and[V, R : Conjunction : ExtractBoolean, P](
    first: Expr[V, R, P],
    second: Expr[V, R, P],
    remaining: Expr[V, R, P]*,
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.And[V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.And(subExpressions, captureResult)
  }

  def or[V, R : Disjunction : ExtractBoolean, P](
    first: Expr[V, R, P],
    second: Expr[V, R, P],
    remaining: Expr[V, R, P]*,
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.Or[V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.Or(subExpressions, captureResult)
  }

  def not[V, R : Negation, P](
    sub: Expr[V, R, P],
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.Not[V, R, P] =
    Expr.Not(sub, captureResult)

  def when[V, R, P](condExpr: CondExpr[V, P]): WhenExprBuilder[V, P] = new WhenExprBuilder(condExpr)

  def factsOfType[T, P](
    factTypeSet: FactTypeSet[T],
  )(implicit
    captureInput: CaptureFromFacts[T, P],
    captureAllResults: CaptureRootExpr[Seq[TypedFact[T]], P],
  ): FoldableExprBuilder[FactTable, Seq, TypedFact[T], P] =
    new FoldableExprBuilder(
      Expr.WithFactsOfType[T, Seq[TypedFact[T]], P](
        factTypeSet,
        input(captureInput),
        captureAllResults,
      ),
    )

  def withFactsOfType[T, P](
    factTypeSet: FactTypeSet[T],
  )(implicit
    captureInput: CaptureFromFacts[T, P],
  ): WithFactsOfTypeBuilder[T, P] =
    new WithFactsOfTypeBuilder(factTypeSet)

  def collectSome[V, M[_] : Foldable, U, R : Monoid, P](
    inputExpr: Expr[V, M[U], P],
    collectExpr: ValExpr[U, Option[R], P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.CollectFromOutput[V, M, U, R, P] =
    Expr.CollectFromOutput(inputExpr, collectExpr, capture)

  def selectFrom[V, S, R, P](
    inputExpr: Expr[V, S, P],
    lens: NamedLens[S, R],
  )(implicit
    captureInput: CaptureP[V, R, P],
  ): Expr.SelectFromOutput[V, S, R, P] =
    Expr.SelectFromOutput(inputExpr, lens, captureInput)

  def add[V, R : Addition, P](
    lhs: Expr[V, R, P],
    rhs: Expr[V, R, P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.AddOutputs[V, R, P] =
    Expr.AddOutputs(NonEmptyList.of(lhs, rhs), capture)

  def subtract[V, R : Subtraction, P](
    lhs: Expr[V, R, P],
    rhs: Expr[V, R, P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.SubtractOutputs[V, R, P] =
    Expr.SubtractOutputs(NonEmptyList.of(lhs, rhs), capture)

  def negative[V, R : Negative, P](
    inputExpr: Expr[V, R, P],
  )(implicit
    capture: CaptureP[V, R, P],
  ): Expr.NegativeOutput[V, R, P] =
    Expr.NegativeOutput(inputExpr, capture)

  def exists[V, M[_] : Foldable, U, P](
    inputExpr: Expr[V, M[U], P],
    condExpr: ValCondExpr[U, P],
  )(implicit
    capture: CaptureP[V, Boolean, P],
  ): Expr.ExistsInOutput[V, M, U, P] =
    Expr.ExistsInOutput(inputExpr, condExpr, capture)

  def returnInput[V, P](
    implicit
    capture: CaptureP[V, V, P],
  ): Expr.ReturnInput[V, P] =
    Expr.ReturnInput(capture)

  @deprecated("Use returnInput instead", "0.10.0")
  def returnInputFoldable[V, P](
    implicit
    capture: CaptureP[V, V, P],
  ): Expr.ReturnInput[V, P] =
    Expr.ReturnInput(capture)

  @deprecated("Use returnInput instead", "0.10.0")
  def returnInputValue[V, P](
    implicit
    capture: CaptureP[V, V, P],
  ): Expr.ReturnInput[V, P] =
    Expr.ReturnInput(capture)

  def within[V, R, P](
    inputExpr: Expr[V, R, P],
    window: Window[R],
  )(implicit
    capture: CaptureP[V, Boolean, P],
  ): Expr.OutputWithinWindow[V, R, P] =
    Expr.OutputWithinWindow(inputExpr, window, capture)
}
