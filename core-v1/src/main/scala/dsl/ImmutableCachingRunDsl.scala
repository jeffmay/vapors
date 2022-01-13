package com.rallyhealth.vapors.v1

package dsl

import algebra.{CombineHolder, SelectHolder}
import data.{ExprState, FactTable}
import engine.ImmutableCachingEngine
import engine.ImmutableCachingEngine.{CachedResult, ResultCache}

trait ImmutableCachingRunDsl extends RunExprDsl {
  self: DslTypes =>

  override final type RunState = ImmutableCachingEngine.ResultCache
  override final type RunResult[+O] = O
  override final type RunWithResult[+PO, -I, +O] = O

  private def visitExprReturningCache[PO <: I, I, O](
    expr: I ~:> O,
    initInput: ExprState.Output[PO],
    initState: ResultCache,
  ): CachedResult[O] =
    expr.visit(ImmutableCachingEngine[OP](initInput.factTable, initState))(initInput.output)

  override protected final def visitExpr[PO <: I, I, O](
    expr: I ~:> O,
    initInput: ExprState.Output[PO],
    initState: ResultCache,
  ): O =
    visitExprReturningCache(expr, initInput, initState).value

  override protected final def initRunState(factTable: FactTable): ResultCache = ResultCache.empty

  override implicit final def runAny[O](expr: Any ~:> O): RunWithCacheExpr[O] = new RunWithCacheExpr(expr)

  override implicit final def runWith[I, O](expr: I ~:> O): RunWithCacheAndInputExpr[I, O] =
    new RunWithCacheAndInputExpr(expr)

  override implicit final def runCombine[O : OP](
    builder: CombineHolder[Any, Nothing, Any, Nothing, Any, O, OP],
  ): RunWithCacheExpr[O] = new RunWithCacheExpr(builder.toExpr)

  override implicit final def runCombineWith[I, O : OP](
    builder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): RunWithCacheAndInputExpr[I, O] = new RunWithCacheAndInputExpr(builder.toExpr)

  override implicit def runSelect[A, B, O : OP](builder: SelectHolder[Any, A, B, O, OP]): RunWithCacheExpr[O] =
    new RunWithCacheExpr(builder.toExpr)

  override implicit def runSelectWith[I, A, B, O : OP](
    builder: SelectHolder[I, A, B, O, OP],
  ): RunWithCacheAndInputExpr[I, O] =
    new RunWithCacheAndInputExpr(builder.toExpr)

  final class RunWithCacheExpr[+O](expr: Any ~:> O) extends RunExpr[O](expr) {

    override def run(): O = super.run()

    def run(cacheState: ImmutableCachingEngine.ResultCache): O = run(FactTable.empty, cacheState)

    override def run(factTable: FactTable): O = super.run(factTable)

    def run(
      factTable: FactTable,
      cacheState: ImmutableCachingEngine.ResultCache,
    ): O =
      visitExpr(expr, ExprState.Empty(factTable), cacheState)

    def runAndCache(): CachedResult[O] =
      runAndCache(FactTable.empty, ResultCache.empty)

    def runAndCache(cacheState: ImmutableCachingEngine.ResultCache): CachedResult[O] =
      runAndCache(FactTable.empty, cacheState)

    def runAndCache(factTable: FactTable): CachedResult[O] = runAndCache(factTable, ResultCache.empty)

    def runAndCache(
      factTable: FactTable,
      cacheState: ImmutableCachingEngine.ResultCache,
    ): CachedResult[O] = visitExprReturningCache(expr, ExprState.Empty(factTable), cacheState)
  }

  final class RunWithCacheAndInputExpr[-I, +O](expr: I ~:> O) extends RunWithInputExpr[I, O](expr) {

    override def runWith[In <: I](input: In): O = super.runWith(input)

    def runWith[In <: I](
      input: In,
      cacheState: ImmutableCachingEngine.ResultCache,
    ): O =
      runWith(input, FactTable.empty, cacheState)

    override def runWith[In <: I](
      input: In,
      factTable: FactTable,
    ): O = super.runWith(input, factTable)

    def runWith[In <: I](
      input: In,
      factTable: FactTable,
      cacheState: ImmutableCachingEngine.ResultCache,
    ): O =
      visitExpr(expr, ExprState.Output(input, factTable), cacheState)

    def runAndCacheWith[In <: I](input: In): CachedResult[O] =
      runAndCacheWith(input, FactTable.empty, ResultCache.empty)

    def runAndCacheWith[In <: I](
      input: In,
      cacheState: ImmutableCachingEngine.ResultCache,
    ): CachedResult[O] =
      runAndCacheWith(input, FactTable.empty, cacheState)

    def runAndCacheWith[In <: I](
      input: In,
      factTable: FactTable,
    ): CachedResult[O] = runAndCacheWith(input, factTable, ResultCache.empty)

    def runAndCacheWith[In <: I](
      input: In,
      factTable: FactTable,
      cacheState: ImmutableCachingEngine.ResultCache,
    ): CachedResult[O] = visitExprReturningCache(expr, ExprState.Output(input, factTable), cacheState)
  }
}
