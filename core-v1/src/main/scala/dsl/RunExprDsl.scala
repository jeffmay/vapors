package com.rallyhealth.vapors.v1

package dsl

import algebra.CombineHolder
import data.{ExprState, FactTable}

trait RunExprDsl[OP[_]] extends CommonDsl[OP] {

  type Result[+PO, -I, +O]

  protected def visitExpr[PO <: I, I, O](
    expr: I ~> O,
    initState: ExprState[Any, PO],
  ): Result[PO, I, O]

  implicit final def runCombine[O : OP](
    builder: CombineHolder[Nothing, Nothing, Any, Nothing, Any, O, OP],
  ): RunExpr[O] =
    new RunExpr(builder.toExpr)

  implicit final def runCombineWith[I, O : OP](
    builder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): RunWithExpr[I, O] =
    new RunWithExpr(builder.toExpr)

  implicit final class RunExpr[+O](expr: Nothing ~> O) {

    /**
      * Runs the expression without any starting input starting with the given [[FactTable]].
      *
      * @note this requires `()` because this could execute side-effects from the attached debuggers.
      */
    def run(factTable: FactTable = FactTable.empty): Result[Nothing, Nothing, O] = {
      visitExpr[Nothing, Nothing, O](expr, ExprState.Empty(factTable))
    }
  }

  implicit final class RunWithExpr[-I, +O](expr: I ~> O) {

    /**
      * Runs the expression with the given input and starting [[FactTable]].
      */
    def runWith[In <: I](
      input: In,
      factTable: FactTable = FactTable.empty,
    ): Result[In, I, O] = {
      visitExpr(expr, ExprState.Output(input, factTable))
    }
  }

}
