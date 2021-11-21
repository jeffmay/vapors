package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import debug.DebugArgs

trait DebugExprDsl {
  self: DslTypes =>

  /**
    * Uses the compiler-inferred types supplied by the implicit [[DebugArgs]] for a given expression.
    *
    * We only allow attaching debug functions by default by upcasting the [[DebugArgs.Adapter]] to
    * just a [[DebugArgs.Attacher]]. If you want to be able to invoke the
    */
  implicit def debugExpr[E <: Expr.AnyWith[OP]](
    expr: E,
  )(implicit
    debugArgs: DebugArgs[E, OP],
  ): DebugArgs.Attacher[E, OP, debugArgs.In, debugArgs.Out] =
    DebugArgs[OP].of(expr)(debugArgs)
}
