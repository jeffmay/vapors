package com.rallyhealth.vapors.v1

package dsl

import algebra.ExprResult
import data.ExprState
import engine.StandardEngine

trait StandardRunDsl extends RunExprDsl {

  override final type Result[+PO, -I, +O] = ExprResult[PO, I, O, OP]

  override protected def visitExpr[PO <: I, I, O](
    expr: I ~> O,
    initState: ExprState[Any, PO],
  ): ExprResult[PO, I, O, OP] = {
    expr.visit(StandardEngine[OP](initState))(implicitly)
  }
}
