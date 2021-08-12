package com.rallyhealth

package vapors.v1.syntax

import vapors.v1.algebra.Expr
import vapors.v1.math.Add

// TODO: This might not be necessary
final class AddSyntax[IL, L](private val left: Expr[IL, L]) extends AnyVal {

  def +[IR, R, O](right: Expr[IR, R])(implicit add: Add[L, R, O]): Expr[IL with IR, O] =
    Expr.Combine(left, right, add.combine)
}
