package com.rallyhealth

package vapors.v1

import vapors.v1.algebra.{Expr, ExprResult}
import vapors.v1.data.{ExprState, FactTable}
import vapors.v1.engine.InterpretExprAsFn

package object dsl {

  // TODO: Use nicer syntax for this

  def evalWithFactTable[O, OP[_]](expr: Expr[Any, O, OP])(factTable: FactTable): ExprResult[Unit, Nothing, O, OP] = {
    expr.visit(InterpretExprAsFn.Visitor[OP](ExprState.Output((), factTable)))(implicitly)
  }

  def evalWithState[I, O, OP[_]](expr: Expr[I, O, OP])(state: ExprState[I, Nothing]): ExprResult[Unit, I, O, OP] = {
    expr.visit(InterpretExprAsFn.Visitor[OP](state))(implicitly)
  }
}
