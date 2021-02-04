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
  final type CondExpr[F[_], V, P] = Expr[F, V, Boolean, P]

  @deprecated("Use com.rallyhealth.vapors.core.dsl.ValExpr instead.", "0.8.0")
  final type ValExpr[V, R, P] = Expr[Id, V, R, P]

  @deprecated("Use com.rallyhealth.vapors.core.dsl.ValCondExpr instead.", "0.8.0")
  final type ValCondExpr[V, P] = dsl.ValExpr[V, Boolean, P]

  @deprecated("Use com.rallyhealth.vapors.core.dsl.RootExpr instead.", "0.8.0")
  final type RootExpr[R, P] = Expr[Id, FactTable, R, P]
}

// TODO: Remove methods that are not useful anymore and move less-useful methods to a separate object
trait ExprDsl extends WrapExprSyntax {

  def eval[R, P](facts: FactTable)(query: RootExpr[R, P]): ExprResult[Id, FactTable, R, P] = {
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
  ): Expr.ConstOutput[Id, FactTable, R, P] =
    Expr.ConstOutput(value, evidence, capture)

  /**
    * Uses the value of a given fact and also considers the fact as evidence of its own value.
    */
  def factValue[R, P](
    typedFact: TypedFact[R],
  )(implicit
    capture: CaptureRootExpr[R, P],
  ): Expr.ConstOutput[Id, FactTable, R, P] =
    const(typedFact.value, Evidence(typedFact))

  def input[F[_], V, P](
    implicit
    capture: CaptureP[F, V, F[V], P],
  ): Expr.ReturnInput[F, V, P] =
    Expr.ReturnInput(capture)

  def define[T](factType: FactType[T]): DefinitionExprBuilder[T] = new DefinitionExprBuilder(factType)

  def usingDefinitions[F[_], V, R, P](
    definitions: Expr.Definition[P]*,
  )(
    subExpr: Expr[F, V, R, P],
  )(implicit
    captureResult: CaptureP[F, V, R, P],
  ): Expr.UsingDefinitions[F, V, R, P] =
    Expr.UsingDefinitions(definitions.toVector, subExpr, captureResult)

  def embed[F[_], V, R, P](
    expr: RootExpr[R, P],
  )(implicit
    captureResult: CaptureP[F, V, R, P],
  ): Expr.Embed[F, V, R, P] =
    Expr.Embed(expr, captureResult)

  def and[F[_], V, R : Conjunction : ExtractBoolean, P](
    first: Expr[F, V, R, P],
    second: Expr[F, V, R, P],
    remaining: Expr[F, V, R, P]*,
  )(implicit
    captureResult: CaptureP[F, V, R, P],
  ): Expr.And[F, V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.And(subExpressions, captureResult)
  }

  def or[F[_], V, R : Disjunction : ExtractBoolean, P](
    first: Expr[F, V, R, P],
    second: Expr[F, V, R, P],
    remaining: Expr[F, V, R, P]*,
  )(implicit
    captureResult: CaptureP[F, V, R, P],
  ): Expr.Or[F, V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.Or(subExpressions, captureResult)
  }

  def not[F[_], V, R : Negation, P](
    sub: Expr[F, V, R, P],
  )(implicit
    captureResult: CaptureP[F, V, R, P],
  ): Expr.Not[F, V, R, P] =
    Expr.Not(sub, captureResult)

  def when[F[_], V, R, P](condExpr: CondExpr[F, V, P]): WhenExprBuilder[F, V, P] = new WhenExprBuilder(condExpr)

  def factsOfType[T, P](
    factTypeSet: FactTypeSet[T],
  )(implicit
    captureInput: CaptureFromFacts[T, P],
    captureAllResults: CaptureRootExpr[Seq[TypedFact[T]], P],
  ): FoldableExprBuilder[Id, FactTable, Seq, TypedFact[T], P] =
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

  def collectSome[F[_], V, M[_] : Foldable, U, R : Monoid, P](
    inputExpr: Expr[F, V, M[U], P],
    collectExpr: ValExpr[U, Option[R], P],
  )(implicit
    capture: CaptureP[F, V, R, P],
  ): Expr.CollectFromOutput[F, V, M, U, R, P] =
    Expr.CollectFromOutput(inputExpr, collectExpr, capture)

  def selectFrom[F[_], V, S, R, P](
    inputExpr: Expr[F, V, S, P],
    lens: NamedLens[S, R],
  )(implicit
    captureInput: CaptureP[F, V, R, P],
  ): Expr.SelectFromOutput[F, V, S, R, P] =
    Expr.SelectFromOutput(inputExpr, lens, captureInput)

  def add[F[_], V, R : Addition, P](
    lhs: Expr[F, V, R, P],
    rhs: Expr[F, V, R, P],
  )(implicit
    capture: CaptureP[F, V, R, P],
  ): Expr.AddOutputs[F, V, R, P] =
    Expr.AddOutputs(NonEmptyList.of(lhs, rhs), capture)

  def subtract[F[_], V, R : Subtraction, P](
    lhs: Expr[F, V, R, P],
    rhs: Expr[F, V, R, P],
  )(implicit
    capture: CaptureP[F, V, R, P],
  ): Expr.SubtractOutputs[F, V, R, P] =
    Expr.SubtractOutputs(NonEmptyList.of(lhs, rhs), capture)

  def negative[F[_], V, R : Negative, P](
    inputExpr: Expr[F, V, R, P],
  )(implicit
    capture: CaptureP[F, V, R, P],
  ): Expr.NegativeOutput[F, V, R, P] =
    Expr.NegativeOutput(inputExpr, capture)

  def exists[F[_], V, M[_] : Foldable, U, P](
    inputExpr: Expr[F, V, M[U], P],
    condExpr: ValCondExpr[U, P],
  )(implicit
    capture: CaptureP[F, V, Boolean, P],
  ): Expr.ExistsInOutput[F, V, M, U, P] =
    Expr.ExistsInOutput(inputExpr, condExpr, capture)

  def returnInputFoldable[F[_], V, P](
    implicit
    capture: CaptureP[F, V, F[V], P],
  ): Expr.ReturnInput[F, V, P] =
    Expr.ReturnInput(capture)

  def returnInputValue[V, P](
    implicit
    capture: CaptureP[Id, V, V, P],
  ): Expr.ReturnInput[Id, V, P] =
    Expr.ReturnInput[Id, V, P](capture)

  def within[F[_], V, R, P](
    inputExpr: Expr[F, V, R, P],
    window: Window[R],
  )(implicit
    capture: CaptureP[F, V, Boolean, P],
  ): Expr.OutputWithinWindow[F, V, R, P] =
    Expr.OutputWithinWindow(inputExpr, window, capture)
}
