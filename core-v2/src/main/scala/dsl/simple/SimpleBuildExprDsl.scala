package com.rallyhealth.vapors.v2
package dsl.simple

import algebra.Expr
import dsl.BuildExprDsl
import logical.Conjunction

/**
 * Implements the [[BuildExprDsl]] for unwrapped types. While some of the definitions just defer to the
 * parent class, they override method signatures to provide better type messaging in the IDEs and compiler output.
 *
 * See [[SimpleDslTypes]]
 */
trait SimpleBuildExprDsl extends SimpleDslTypes with BuildExprDsl {

  extension [V : OP] (value: V) def const: Expr.Const[V, OP] = Expr.Const(value)

  override def and[I, O : Conjunction : OP](one: I ~:> O, two: I ~:> O): I ~:> O = super.and(one, two)
}
