package com.rallyhealth

package vapors.v1.dsl

import vapors.v1.algebra.Expr
import vapors.v1.syntax.AddSyntax

// TODO: This may not be necessary
trait ArithmeticSyntax {

  implicit def addToCombine[I, O](expr: Expr[I, O]): AddSyntax[I, O] = new AddSyntax(expr)
}
