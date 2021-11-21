package com.rallyhealth.vapors.v1

package dsl

import algebra.CombineHolder
import data.{ExprState, FactTable}

trait RunExprDsl {
  self: DslTypes =>

  type Result[+PO, -I, +O]

  protected def visitExpr[PO <: I, I, O](
    expr: I ~:> O,
    initState: ExprState[Any, PO],
  ): Result[PO, I, O]

  implicit def run[O](expr: Any ~:> O): SpecificRunExpr[O]

  implicit def runWith[I, O](expr: I ~:> O): SpecificRunWithExpr[I, O]

  implicit def runCombine[O : OP](builder: CombineHolder[Any, Nothing, Any, Nothing, Any, O, OP]): SpecificRunExpr[O]

  implicit def runCombineWith[I, O : OP](
    builder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): SpecificRunWithExpr[I, O]

  type SpecificRunExpr[+O] <: RunExpr[O]
  type SpecificRunWithExpr[-I, +O] <: RunWithExpr[I, O]

  class RunExpr[+O](expr: Any ~:> O) {

    /**
      * Runs the expression without any starting input starting with the given [[FactTable]].
      *
      * @note this requires `()` because this could execute side-effects from the attached debuggers.
      */
    def run(factTable: FactTable = FactTable.empty): Result[Nothing, Any, O] = {
      visitExpr[Nothing, Any, O](expr, ExprState.Empty(factTable))
    }
  }

  class RunWithExpr[-I, +O](expr: I ~:> O) {

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
