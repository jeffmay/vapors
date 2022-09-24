package com.rallyhealth.vapors.v1

package dsl

import algebra.{CombineHolder, Expr, SelectHolder}
import debug.DebugArgs

trait DebugExprDsl {
  self: DslTypes =>

  /**
    * Allows calling [[WidenExpr.widen]] on an expression to widen the type.
    *
    * This is useful when using an IDE to generate the type for an expression when you don't want to know
    * the most specific subclass of [[Expr]].
    */
  implicit def widenExpr[I, O](expr: I ~:> O): WidenExpr[I, ~:>, O] = new WidenExpr[I, ~:>, O](expr)

  /**
    * Same as [[widenExpr]], but for [[CombineHolder]]s.
    */
  implicit def widenCombineHolder[I, O : OP](
    expr: CombineHolder[I, Nothing, Any, Nothing, Any, O, OP],
  ): WidenExpr[I, ~:>, O] = new WidenExpr[I, ~:>, O](expr.toExpr)

  /**
    * Same as [[widenExpr]], but for [[SelectHolder]]s.
    */
  implicit def widenSelectHolder[I, A, B, O : OP](expr: SelectHolder[I, A, B, O, OP]): WidenExpr[I, ~:>, O] =
    new WidenExpr[I, ~:>, O](expr.toExpr)

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

final class WidenExpr[I, DSLExpr[-_, +_], O](private val expr: DSLExpr[I, O]) extends AnyVal {

  def widen: DSLExpr[I, O] = expr
}
