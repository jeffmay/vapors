package com.rallyhealth.vapors.v1

package dsl

import algebra.CombineHolder
import data.{ExprState, FactTable}
import engine.SimpleEngine

trait SimpleRunDsl extends RunExprDsl {
  self: DslTypes =>

  override final type RunWithResult[+PO, -I, +O] = O

  override protected final def visitExpr[PO <: I, I, O](
    expr: I ~:> O,
    initState: ExprState[Any, PO],
  ): O = expr.visit(SimpleEngine[OP](initState.factTable))(initState.output)

  override implicit final def run[O](expr: Any ~:> O): RunIdExpr[O] = new RunIdExpr(expr)

  override implicit final def runWith[I, O](expr: I ~:> O): RunWithIdExpr[I, O] = new RunWithIdExpr(expr)

  override implicit final def runCombine[O : OP](
    builder: CombineHolder[Any, Nothing, Any, Nothing, Any, O, OP],
  ): RunIdExpr[O] = new RunIdExpr(builder.toExpr)

  override implicit final def runCombineWith[I, O : OP](
    builder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): RunWithIdExpr[I, O] = new RunWithIdExpr(builder.toExpr)

  override final type SpecificRunExpr[+O] = RunIdExpr[O]
  override final type SpecificRunWithExpr[-I, +O] = RunWithIdExpr[I, O]

  final class RunIdExpr[+O](expr: Any ~:> O) extends RunExpr[O](expr) {
    override def run(factTable: FactTable = FactTable.empty): O = super.run(factTable)
  }

  final class RunWithIdExpr[-I, +O](expr: I ~:> O) extends RunWithExpr[I, O](expr) {
    override def runWith[In <: I](
      input: In,
      factTable: FactTable = FactTable.empty,
    ): O = super.runWith(input, factTable)
  }
}
