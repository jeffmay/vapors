package com.rallyhealth.vapors.core.dsl

import cats._
import com.rallyhealth.vapors.core.algebra.{CaptureP, Expr, ExprSorter}
import com.rallyhealth.vapors.core.data.{Evidence, TypedFact, Window}
import com.rallyhealth.vapors.core.lens.NamedLens
import com.rallyhealth.vapors.core.math.{Addition, Negative, Subtraction}

import scala.collection.Factory
import scala.reflect.runtime.universe.TypeTag

sealed class ExprBuilder[V, M[_], U, P](val returnOutput: Expr[V, M[U], P]) {

  type CaptureResult[R] = CaptureP[V, R, P]

  type CaptureInput[G[_]] = CaptureP[V, G[V], P]
  type CaptureAllInputCond = CaptureResult[Boolean]

  type CaptureEachOutputResult[R] = CaptureP[U, R, P]
  type CaptureEachOutput = CaptureEachOutputResult[U]
  type CaptureEachOutputCond = CaptureEachOutputResult[Boolean]

  def returnInput(implicit captureInput: CaptureResult[V]): Expr[V, V, P] = Expr.ReturnInput(captureInput)

  def embed[R](expr: Expr[M[U], R, P]): ExprBuilder[M[U], Id, R, P] = new ExprBuilder[M[U], Id, R, P](expr)
}

object ExprBuilder {

  type ValId[V, P] = ValExprBuilder[V, V, P]
  type ValFn[V, R, P] = ValExprBuilder[V, V, P] => ExprBuilder[V, Id, R, P]

  type FoldableId[F[_], V, P] = FoldableExprBuilder[F[V], F, V, P]
  type FoldableFn[F[_], V, M[_], U, P] = FoldableId[F, V, P] => ExprBuilder[F[V], M, U, P]
}

trait ExprBuilderSyntax {

  /**
    * Implicitly allows embedding any expression that only requires the FactTable into expression.
    *
    * @see [[Expr.Embed]]
    */
  implicit def embedExpr[V, R, P](
    expr: RootExpr[R, P],
  )(implicit
    captureResult: CaptureP[V, R, P],
  ): Expr.Embed[V, R, P] = Expr.Embed(expr, captureResult)

  implicit def liftValExpr[V, R, P](expr: Expr[V, R, P]): ValExprBuilder[V, R, P] =
    new ValExprBuilder(expr)

  implicit def returnFoldableExprOutput[V, M[_], U, P](builder: FoldableExprBuilder[V, M, U, P]): Expr[V, M[U], P] =
    builder.returnOutput

  implicit def returnValExprOutput[V, R, P](builder: ValExprBuilder[V, R, P]): Expr[V, R, P] =
    builder.returnOutput

  implicit def addTo[R : Addition](lhs: R): AdditionBuilderOps[R] = new AdditionBuilderOps(lhs)

  implicit def subtractFrom[R : Subtraction](lhs: R): SubtractBuilderOps[R] = new SubtractBuilderOps(lhs)
}

final class AdditionBuilderOps[R : Addition](number: R) {

  def +[V, P](
    builder: ValExprBuilder[V, R, P],
  )(implicit
    captureResult: builder.CaptureResult[R],
  ): ValExprBuilder[V, R, P] = {
    builder.addTo(number)
  }
}

final class SubtractBuilderOps[R : Subtraction](number: R) {

  def -[V, P](
    builder: ValExprBuilder[V, R, P],
  )(implicit
    captureResult: builder.CaptureResult[R],
  ): ValExprBuilder[V, R, P] = {
    builder.subtractFrom(number)
  }
}

sealed class FoldableExprBuilder[V, M[_] : Foldable, U, P](returnOutput: Expr[V, M[U], P])
  extends ExprBuilder[V, M, U, P](returnOutput) {

  def toList(
    implicit
    ev: M[U] <:< Iterable[U],
    captureOutput: CaptureP[V, List[U], P],
  ): FoldableExprBuilder[V, List, U, P] =
    to(List)

  def toSet(
    implicit
    ev: M[U] <:< Iterable[U],
    captureOutput: CaptureP[V, Set[U], P],
  ): FoldableExprBuilder[V, Set, U, P] = {
    to(Set)
  }

  def to[N[_] : Foldable](
    factory: Factory[U, N[U]],
  )(implicit
    ev: M[U] <:< Iterable[U],
    captureOutput: CaptureP[V, N[U], P],
  ): FoldableExprBuilder[V, N, U, P] =
    new FoldableExprBuilder({
      Expr.SelectFromOutput[V, M[U], N[U], P](
        returnOutput,
        NamedLens.id[M[U]].asIterable.to(factory),
        captureOutput,
      )
    })

  def sorted(
    implicit
    orderU: Order[U],
    ev: M[U] <:< Seq[U],
    tt: TypeTag[U],
    factory: Factory[U, M[U]],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[V, M, U, P] =
    new FoldableExprBuilder(Expr.SortOutput(returnOutput, ExprSorter.byNaturalOrder[M, U], captureResult))

  def sortBy[R : Order](
    buildLens: NamedLens.Fn[U, R],
  )(implicit
    ev: M[U] <:< Seq[U],
    tt: TypeTag[U],
    factory: Factory[U, M[U]],
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[V, M, U, P] = {
    val lens = buildLens(NamedLens.id[U])
    new FoldableExprBuilder(Expr.SortOutput(returnOutput, ExprSorter.byField[M, U, R](lens), captureResult))
  }

  def take(
    n: Int,
  )(implicit
    traverseM: Traverse[M],
    traverseFilterM: TraverseFilter[M],
    captureAllOutput: CaptureP[V, M[U], P],
  ): FoldableExprBuilder[V, M, U, P] =
    new FoldableExprBuilder(Expr.TakeFromOutput(returnOutput, n, captureAllOutput))

  def headOption(
    implicit
    traverseM: Traverse[M],
    traverseFilterM: TraverseFilter[M],
    ev: M[U] <:< Iterable[U],
    captureAllOutput: CaptureP[V, M[U], P],
    captureHeadOutput: CaptureP[V, Option[U], P],
  ): FoldableExprBuilder[V, Option, U, P] =
    new FoldableExprBuilder(
      Expr.SelectFromOutput(take(1), NamedLens.id[M[U]].headOption, captureHeadOutput),
    )

  def map[R](
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[U, Id, R, P],
  )(implicit
    functorM: Functor[M],
    postEachOutput: CaptureP[U, U, P],
    postMap: CaptureP[V, M[R], P],
  ): FoldableExprBuilder[V, M, R, P] = {
    val mapExpr = buildFn(
      new ValExprBuilder(Expr.ReturnInput(postEachOutput)),
    )
    val next = Expr.MapOutput(returnOutput, mapExpr.returnOutput, postMap)
    new FoldableExprBuilder(next)
  }

  def flatMap[X](
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[U, M, X, P],
  )(implicit
    flatMapM: FlatMap[M],
    postEachOutput: CaptureP[U, U, P],
    postFlatMap: CaptureP[V, M[X], P],
  ): FoldableExprBuilder[V, M, X, P] = {
    val flatMapExpr = buildFn(
      new ValExprBuilder(Expr.ReturnInput(postEachOutput)),
    )
    val next = Expr.FlatMapOutput(returnOutput, flatMapExpr.returnOutput, postFlatMap)
    new FoldableExprBuilder(next)
  }

  def isEmpty(
    implicit
    captureResult: CaptureAllInputCond,
  ): FoldInExprBuilder[V, Boolean, P] =
    new FoldInExprBuilder(Expr.OutputIsEmpty(returnOutput, captureResult))

  def exists(
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[U, Id, Boolean, P],
  )(implicit
    postEachOutput: CaptureP[U, U, P],
    captureResult: CaptureAllInputCond,
  ): FoldInExprBuilder[V, Boolean, P] = {
    val condExpr = buildFn(new ValExprBuilder(Expr.ReturnInput(postEachOutput)))
    val next = Expr.ExistsInOutput(returnOutput, condExpr.returnOutput, captureResult)
    new FoldInExprBuilder(next)
  }

  def filter(
    buildFn: ValExprBuilder[U, U, P] => ExprBuilder[U, Id, Boolean, P],
  )(implicit
    filterM: FunctorFilter[M],
    captureEachOutput: CaptureEachOutput,
    captureResult: CaptureResult[M[U]],
  ): FoldableExprBuilder[V, M, U, P] = {
    val condExpr = buildFn(
      new ValExprBuilder(Expr.ReturnInput[U, P](captureEachOutput)),
    )
    new FoldableExprBuilder(Expr.FilterOutput(returnOutput, condExpr.returnOutput, captureResult))
  }

  def containsAny(
    validValues: Set[U],
  )(implicit
    filterM: FunctorFilter[M],
    captureEachOutput: CaptureEachOutput,
    captureEachOutputCond: CaptureEachOutputCond,
    captureResult: CaptureResult[M[U]],
    captureCond: CaptureAllInputCond,
  ): FoldInExprBuilder[V, Boolean, P] =
    new FoldInExprBuilder(
      Expr.Not(
        filter(_ in validValues).isEmpty,
        captureCond,
      ),
    )
}

final class FoldInExprBuilder[V, R, P](returnOutput: Expr[V, R, P])
  extends FoldableExprBuilder[V, Id, R, P](returnOutput)

final class FoldOutExprBuilder[V, M[_] : Foldable, U, P](returnOutput: Expr[V, M[U], P])
  extends FoldableExprBuilder[V, M, U, P](returnOutput)

// TODO: Combine with FoldIn / FoldOut?
final class ValExprBuilder[V, R, P](returnOutput: Expr[V, R, P]) extends ExprBuilder[V, Id, R, P](returnOutput) {

  @inline private def buildGetExpr[N[_], X](
    buildLens: NamedLens.Fn[R, N[X]],
  )(implicit
    captureResult: CaptureP[V, N[X], P],
  ): Expr[V, N[X], P] = {
    val lens = buildLens(NamedLens.id[R])
    // if the previous node was a SelectFromOutput, then combine the lenses and produce a single node
    returnOutput match {
      case prev: Expr.SelectFromOutput[V, s, R, P] =>
        // capture the starting type as an existential type parameter 's'
        // it is ignored in the return type after the compile proves that this code is safe
        Expr.SelectFromOutput[V, s, N[X], P](prev.inputExpr, prev.lens.andThen(lens), captureResult)
      case _ =>
        // otherwise, build the lens as a new SelectFromOutput node
        Expr.SelectFromOutput(returnOutput, lens, captureResult)
    }
  }

  def get[X](
    buildLens: NamedLens.Fn[R, X],
  )(implicit
    captureResult: CaptureP[V, X, P],
  ): ValExprBuilder[V, X, P] =
    new ValExprBuilder(buildGetExpr[Id, X](buildLens))

  def getFoldable[N[_] : Foldable, X](
    buildLens: NamedLens.Fn[R, N[X]],
  )(implicit
    captureResult: CaptureP[V, N[X], P],
  ): FoldOutExprBuilder[V, N, X, P] =
    new FoldOutExprBuilder(buildGetExpr(buildLens))

  def value[X](
    implicit
    ev: R <:< TypedFact[X],
    captureResult: CaptureResult[X],
  ): ValExprBuilder[V, X, P] =
    new ValExprBuilder(buildGetExpr[Id, X](_.field("value", _.value)))

  def in(accepted: Set[R])(implicit captureOutput: CaptureResult[Boolean]): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(Expr.OutputWithinSet(returnOutput, accepted, captureOutput))

  def add(
    rhs: R,
  )(implicit
    R: Addition[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    this + rhs

  def +(
    rhs: R,
  )(implicit
    R: Addition[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.add(returnOutput, Expr.ConstOutput(rhs, Evidence.none, captureResult)))

  def addTo(
    lhs: R,
  )(implicit
    R: Addition[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.add(Expr.ConstOutput(lhs, Evidence.none, captureResult), returnOutput))

  def subtract(
    rhs: R,
  )(implicit
    R: Subtraction[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    this - rhs

  def -(
    rhs: R,
  )(implicit
    R: Subtraction[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.subtract(returnOutput, Expr.ConstOutput(rhs, Evidence.none, captureResult)))

  def subtractFrom(
    lhs: R,
  )(implicit
    R: Subtraction[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.subtract(Expr.ConstOutput(lhs, Evidence.none, captureResult), returnOutput))

  def unary_-(
    implicit
    R: Negative[R],
    captureResult: CaptureResult[R],
  ): ValExprBuilder[V, R, P] =
    new ValExprBuilder(ExprDsl.negative(returnOutput))

  def within(
    window: Window[R],
  )(implicit
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    new ValExprBuilder(ExprDsl.within(returnOutput, window))

  def isEqualTo(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.equalTo(value))

  def ===(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.equalTo(value))

  def !==(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    Expr.Not(within(Window.equalTo(value)), captureResult)

  def <(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.lessThan(value))

  def <=(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.lessThanOrEqual(value))

  def >(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.greaterThan(value))

  def >=(
    value: R,
  )(implicit
    orderR: Order[R],
    captureResult: CaptureAllInputCond,
  ): ValExprBuilder[V, Boolean, P] =
    within(Window.greaterThanOrEqual(value))
}
