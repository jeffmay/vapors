package com.rallyhealth.vapors.v1

package dsl

import algebra.{CombineHolder, SelectHolder}
import data.{ExprState, FactTable}

trait RunExprDsl {
  self: DslTypes =>

  type RunWithResult[+PO, -I, +O]
  type RunResult[+O] = RunWithResult[Nothing, Nothing, O]

  protected def visitExpr[PO <: I, I, O](
    expr: I ~:> O,
    initState: ExprState[Any, PO],
  ): RunWithResult[PO, I, O]

  implicit def runAny[O](expr: Any ~:> O): SpecificRunExpr[O]

  implicit def runWith[I, O](expr: I ~:> O): SpecificRunWithExpr[I, O]

  implicit def runCombine[O : OP](builder: CombineHolder[Any, Nothing, Any, Nothing, Any, O, OP]): SpecificRunExpr[O]

  implicit def runCombineWith[I, O : OP](
    builder: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): SpecificRunWithExpr[I, O]

  implicit def runSelect[A, B, O : OP](builder: SelectHolder[Any, A, B, O, OP]): SpecificRunExpr[O]

  implicit def runSelectWith[I, A, B, O : OP](builder: SelectHolder[I, A, B, O, OP]): SpecificRunWithExpr[I, O]

  type SpecificRunExpr[+O] <: RunExpr[O]
  type SpecificRunWithExpr[-I, +O] <: RunWithExpr[I, O]

  /**
    * The expression needs to be able to handle any input, even though we will not give it any input.
    *
    * Technically, the input for this expression would be `ExprState[Nothing, Nothing]`, and thus the expression
    * could be `Nothing ~> O`, however, this would permit every expression to be run (because `Nothing` can be
    * passed into any expression's input, as it will always be a subtype of the required input). However, at
    * runtime, `Nothing` would throw an exception if the expression attempted to use the initial value and this
    * is probably not what the caller intends. Instead, we distinguish between what is provided (i.e. `Nothing`)
    * and what is required (i.e. `Any`). The expression must be the most specific kind of expression that can
    * handle any input, but the type of input we will provide to the runner is `ExprState[Nothing, Nothing]`,
    * which -- by definition -- this expression must be able to handle.
    */
  class RunExpr[+O](expr: Any ~:> O) {

    /**
      * Runs the expression without any starting input starting with the given [[FactTable]].
      *
      * @note this requires `()` because this could execute side-effects from the attached debuggers.
      */
    def run(factTable: FactTable = FactTable.empty): RunResult[O] = {
      visitExpr[Nothing, Nothing, O](expr, ExprState.Empty(factTable))
    }
  }

  class RunWithExpr[-I, +O](expr: I ~:> O) {

    /**
      * Runs the expression with the given input and starting [[FactTable]].
      */
    def runWith[In <: I](
      input: In,
      factTable: FactTable = FactTable.empty,
    ): RunWithResult[In, I, O] = {
      visitExpr(expr, ExprState.Output(input, factTable))
    }
  }

}
