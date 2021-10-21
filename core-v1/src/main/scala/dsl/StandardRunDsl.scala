package com.rallyhealth.vapors.v1

package dsl

import algebra.{CombineHolder, ExprResult}
import data.{ExprState, FactTable}
import engine.StandardEngine

trait StandardRunDsl extends RunExprDsl {
  self: DslTypes =>

  override final type Result[+PO, -I, +O] = ExprResult[PO, I, O, OP]

  override protected def visitExpr[PO <: I, I, O](
    expr: I ~> O,
    initState: ExprState[Any, PO],
  ): ExprResult[PO, I, O, OP] = {
    expr.visit(StandardEngine[OP](initState))(implicitly)
  }

  override implicit final def run[O](expr: Any ~> O): RunStandardExpr[O] = new RunStandardExpr(expr)

  override implicit final def runWith[I, O](expr: I ~> O): RunWithStandardExpr[I, O] = new RunWithStandardExpr(expr)

  override implicit def runCombine[O : OP](
    builder: CombineHolder[Any, Nothing, Any, Nothing, Any, O, OP],
  ): RunStandardExpr[O] = new RunStandardExpr(builder.toExpr)

  override implicit def runCombineWith[I, O : OP](
    builder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): RunWithStandardExpr[I, O] = new RunWithStandardExpr(builder.toExpr)

  override type SpecificRunExpr[+O] = RunStandardExpr[O]
  override type SpecificRunWithExpr[-I, +O] = RunWithStandardExpr[I, O]

  final class RunStandardExpr[+O](expr: Any ~> O) extends RunExpr(expr) {
    override def run(factTable: FactTable = FactTable.empty): ExprResult[Nothing, Any, O, OP] = super.run(factTable)
  }

  final class RunWithStandardExpr[-I, +O](expr: I ~> O) extends RunWithExpr(expr) {
    override def runWith[In <: I](
      input: In,
      factTable: FactTable = FactTable.empty,
    ): ExprResult[In, I, O, OP] = super.runWith(input, factTable)
  }
}
