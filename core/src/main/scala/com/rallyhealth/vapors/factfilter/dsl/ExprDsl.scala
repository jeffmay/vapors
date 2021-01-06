package com.rallyhealth.vapors.factfilter.dsl

import cats.data.NonEmptyList
import cats.{Foldable, Id, Monoid}
import com.rallyhealth.vapors.core.algebra.{ConditionBranch, Expr, ExprResult}
import com.rallyhealth.vapors.core.data.{NamedLens, Window}
import com.rallyhealth.vapors.core.logic.{Conjunction, Disjunction, Negation}
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}
import com.rallyhealth.vapors.factfilter.data._
import com.rallyhealth.vapors.factfilter.evaluator.InterpretExprAsFunction

object ExprDsl extends ExprBuilderSyntax with ExprBuilderCatsInstances {

  final type CondExpr[F[_], V, P] = Expr[F, V, Boolean, P]

  final type ValExpr[V, R, P] = Expr[Id, V, R, P]
  final type ValCondExpr[V, P] = ValExpr[V, Boolean, P]

  type RootExpr[R, P] = Expr[Id, FactTable, R, P]

  type CaptureRootExpr[R, P] = CaptureP[Id, FactTable, R, P]
  type CaptureFromFacts[T, P] = CaptureP[Seq, TypedFact[T], Seq[TypedFact[T]], P]

  import InterpretExprAsFunction._

  def eval[R, P](facts: FactTable)(query: RootExpr[R, P]): ExprResult[Id, FactTable, R, P] = {
    InterpretExprAsFunction(query)(Input.fromFactTable(facts))
  }

  /**
    * Lifts the given value into the output of an expression with no evidence.
    */
  def const[R, P](
    value: R,
    evidence: Evidence = Evidence.none,
  )(implicit
    post: CaptureRootExpr[R, P],
  ): RootExpr[R, P] =
    Expr.ConstOutput(value, evidence, post)

  /**
    * Uses the value of a given fact and also considers the fact as evidence of its own value.
    */
  def factValue[R, P](
    typedFact: TypedFact[R],
  )(implicit
    post: CaptureRootExpr[R, P],
  ): RootExpr[R, P] =
    const(typedFact.value, Evidence(typedFact))

  def input[F[_], V, P](
    implicit
    post: CaptureP[F, V, F[V], P],
  ): Expr[F, V, F[V], P] =
    Expr.ReturnInput(post)

  def define[T](factType: FactType[T]): DefinitionBuilder[T] = new DefinitionBuilder(factType)

  final class DefinitionBuilder[T](private val factType: FactType[T]) extends AnyVal {

    def from[P](
      defExpr: RootExpr[T, P],
    )(implicit
      postResult: CaptureRootExpr[FactSet, P],
    ): Expr.Define[Id, T, P] =
      Expr.Define[Id, T, P](factType, defExpr, postResult)

    def fromEvery[M[_] : Foldable, P](
      defExpr: RootExpr[M[T], P],
    )(implicit
      postResult: CaptureRootExpr[FactSet, P],
    ): Expr.Define[M, T, P] =
      Expr.Define(factType, defExpr, postResult)
  }

  def usingDefinitions[F[_], V, R, P](
    definitions: Expr.Definition[P]*,
  )(
    subExpr: Expr[F, V, R, P],
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] = {
    Expr.UsingDefinitions(definitions.toVector, subExpr, postResult)
  }

  /**
    * Implicitly allows embedding any expression that only requires the FactTable into expression.
    *
    * @see [[Expr.Embed]]
    */
  implicit def embed[F[_], V, R, P](
    expr: RootExpr[R, P],
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] = Expr.Embed(expr, postResult)

  def and[F[_], V, R : Conjunction : ExtractBoolean, P](
    first: Expr[F, V, R, P],
    second: Expr[F, V, R, P],
    remaining: Expr[F, V, R, P]*,
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.And(subExpressions, postResult)
  }

  def or[F[_], V, R : Disjunction : ExtractBoolean, P](
    first: Expr[F, V, R, P],
    second: Expr[F, V, R, P],
    remaining: Expr[F, V, R, P]*,
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] = {
    val subExpressions = first :: NonEmptyList.of(second, remaining: _*)
    Expr.Or(subExpressions, postResult)
  }

  def not[F[_], V, R : Negation, P](
    sub: Expr[F, V, R, P],
  )(implicit
    postResult: CaptureP[F, V, R, P],
  ): Expr.Not[F, V, R, P] =
    Expr.Not(sub, postResult)

  def when[F[_], V, R, P](condExpr: CondExpr[F, V, P]): WhenBuilder[F, V, P] = new WhenBuilder(condExpr)

  final class WhenBuilder[F[_], V, P](private val whenExpr: CondExpr[F, V, P]) extends AnyVal {

    def thenReturn[R](thenExpr: Expr[F, V, R, P]): ElseDefaultBuilder[F, V, R, P] =
      new ElseDefaultBuilder(NonEmptyList.of(ConditionBranch(whenExpr, thenExpr)))
  }

  final class ElifBuilder[F[_], V, R, P](
    private val t: (CondExpr[F, V, P], NonEmptyList[ConditionBranch[F, V, R, P]]),
  ) extends AnyVal {

    def thenReturn(thenExpr: Expr[F, V, R, P]): ElseDefaultBuilder[F, V, R, P] =
      new ElseDefaultBuilder(t._2 ::: NonEmptyList.of(ConditionBranch(t._1, thenExpr)))
  }

  final class ElseDefaultBuilder[F[_], V, R, P](private val branches: NonEmptyList[ConditionBranch[F, V, R, P]])
    extends AnyVal {

    def elseReturn(
      elseExpr: Expr[F, V, R, P],
    )(implicit
      postResult: CaptureP[F, V, R, P],
    ): Expr[F, V, R, P] =
      Expr.When(branches, elseExpr, postResult)

    def elif(elifExpr: CondExpr[F, V, P]): ElifBuilder[F, V, R, P] = new ElifBuilder((elifExpr, branches))
  }

  def withFactsOfType[T, P](
    factTypeSet: FactTypeSet[T],
  )(implicit
    postInput: CaptureFromFacts[T, P],
  ): WithFactsOfTypeBuilder[T, P] =
    new WithFactsOfTypeBuilder(factTypeSet)

  final class WithFactsOfTypeBuilder[T, P](
    factTypeSet: FactTypeSet[T],
  )(implicit
    postInput: CaptureFromFacts[T, P],
  ) {

    def where[M[_], U](
      buildSubExpr: ExprBuilder.FoldableFn[Seq, TypedFact[T], M, U, P],
    )(implicit
      postResult: CaptureRootExpr[M[U], P],
    ): RootExpr[M[U], P] =
      Expr.WithFactsOfType(
        factTypeSet,
        buildSubExpr(new FoldableExprBuilder(ExprDsl.input)).returnOutput,
        postResult,
      )

    def returnInput(
      implicit
      postInput: CaptureFromFacts[T, P],
      postResult: CaptureRootExpr[Seq[TypedFact[T]], P],
    ): RootExpr[Seq[TypedFact[T]], P] =
      Expr.WithFactsOfType(factTypeSet, input(postInput), postResult)
  }

  def collectSome[F[_], V, M[_] : Foldable, U, R : Monoid, P](
    inputExpr: Expr[F, V, M[U], P],
    collectExpr: ValExpr[U, Option[R], P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.CollectFromOutput(inputExpr, collectExpr, post)

  def selectFrom[F[_], V, S, R, P](
    inputExpr: Expr[F, V, S, P],
    lens: NamedLens[S, R],
  )(implicit
    postInput: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.SelectFromOutput(inputExpr, lens, postInput)

  def add[F[_], V, R : Addition, P](
    lhs: Expr[F, V, R, P],
    rhs: Expr[F, V, R, P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.AddOutputs(NonEmptyList.of(lhs, rhs), post)

  def subtract[F[_], V, R : Subtraction, P](
    lhs: Expr[F, V, R, P],
    rhs: Expr[F, V, R, P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.SubtractOutputs(NonEmptyList.of(lhs, rhs), post)

  def negative[F[_], V, R : Negative, P](
    inputExpr: Expr[F, V, R, P],
  )(implicit
    post: CaptureP[F, V, R, P],
  ): Expr[F, V, R, P] =
    Expr.NegativeOutput(inputExpr, post)

  def exists[F[_], V, M[_] : Foldable, U, P](
    inputExpr: Expr[F, V, M[U], P],
    condExpr: ValCondExpr[U, P],
  )(implicit
    post: CaptureP[F, V, Boolean, P],
  ): CondExpr[F, V, P] =
    Expr.ExistsInOutput(inputExpr, condExpr, post)

  def returnInputFoldable[F[_], V, P](
    implicit
    post: CaptureP[F, V, F[V], P],
  ): Expr[F, V, F[V], P] =
    Expr.ReturnInput(post)

  def returnInputValue[V, P](
    implicit
    post: CaptureP[Id, V, V, P],
  ): Expr[Id, V, V, P] =
    Expr.ReturnInput[Id, V, P](post)

  def within[F[_], V, R, P](
    inputExpr: Expr[F, V, R, P],
    window: Window[R],
  )(implicit
    post: CaptureP[F, V, Boolean, P],
  ): Expr[F, V, Boolean, P] =
    Expr.OutputWithinWindow(inputExpr, window, post)
}
