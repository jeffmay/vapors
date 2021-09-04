package com.rallyhealth

package vapors.v1.syntax

import vapors.v1.algebra.Expr
import vapors.v1.math.Add

// TODO: This might not be necessary
//final class AddSyntax[LI, LO, PO[_]](private val left: Expr[LI, LO, PO]) extends AnyVal {
//
//  def +[RI <: LI, RO : PO, AO : PO](
//    right: Expr[RI, RO, PO],
//  )(implicit
//    add: Add[LO, RO, AO],
//    opL: PO[LO],
//  ): Expr[RI, AO, PO] =
//    Expr.Combine(left, right, add.combine)
//}
