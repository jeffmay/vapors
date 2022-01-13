package com.rallyhealth.vapors.v1

package dsl

import data.ExprState
import engine.SimpleEngine

trait SimpleRunDsl extends UnwrappedRunDsl {
  self: DslTypes =>

  override protected final def visitExpr[PO <: I, I, O](
    expr: I ~:> O,
    initInput: ExprState.Output[PO],
    initState: Unit,
  ): O = expr.visit(SimpleEngine[OP](initInput.factTable))(initInput.output)
}
