package com.rallyhealth.vapors.v1

package dsl

import algebra.{CombineHolder, ExprResult, SelectHolder}
import data.{ExprState, FactTable}
import engine.StandardEngine

trait StandardRunDsl extends StatelessRunDsl {
  self: DslTypes =>

  override final type RunWithResult[+PO, -I, +O] = ExprResult[PO, I, O, OP]

  override protected final def visitExpr[PO <: I, I, O](
    expr: I ~:> O,
    initInput: ExprState.Output[PO],
    initState: RunState,
  ): ExprResult[PO, I, O, OP] = {
    expr.visit(StandardEngine[OP](initInput))(implicitly)
  }

  override implicit final def runAny[O](expr: Any ~:> O): RunStandardExpr[O] = new RunStandardExpr(expr)

  override implicit final def runWith[I, O](expr: I ~:> O): RunWithStandardExpr[I, O] = new RunWithStandardExpr(expr)

  override implicit def runCombine[O : OP](
    builder: CombineHolder[Any, Nothing, Any, Nothing, Any, O, OP],
  ): RunStandardExpr[O] = new RunStandardExpr(builder.toExpr)

  override implicit def runCombineWith[I, O : OP](
    builder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): RunWithStandardExpr[I, O] = new RunWithStandardExpr(builder.toExpr)

  override implicit def runSelect[A, B, O : OP](builder: SelectHolder[Any, A, B, O, OP]): RunStandardExpr[O] =
    new RunStandardExpr(builder.toExpr)

  override implicit def runSelectWith[I, A, B, O : OP](
    builder: SelectHolder[I, A, B, O, OP],
  ): RunWithStandardExpr[I, O] =
    new RunWithStandardExpr(builder.toExpr)

  final class RunStandardExpr[+O](expr: Any ~:> O) extends RunExpr(expr) {
    override def run(): RunResult[O] = super.run()
    override def run(factTable: FactTable): ExprResult[Nothing, Nothing, O, OP] = super.run(factTable)
  }

  final class RunWithStandardExpr[-I, +O](expr: I ~:> O) extends RunWithInputExpr(expr) {
    override def runWith[In <: I](input: In): ExprResult[In, I, O, OP] = super.runWith(input)
    override def runWith[In <: I](
      input: In,
      factTable: FactTable,
    ): ExprResult[In, I, O, OP] = super.runWith(input, factTable)
  }
}
