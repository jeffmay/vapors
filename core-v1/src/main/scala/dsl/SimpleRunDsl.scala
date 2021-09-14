package com.rallyhealth.vapors.v1

package dsl

import data.ExprState
import engine.SimpleEngine

trait SimpleRunDsl extends RunExprDsl {

  override type Result[+PO, -I, +O] = O

  override protected def visitExpr[PO <: I, I, O](
    expr: I ~> O,
    initState: ExprState[Any, PO],
  ): O = expr.visit(SimpleEngine[OP](initState.factTable))(initState.output)
}
