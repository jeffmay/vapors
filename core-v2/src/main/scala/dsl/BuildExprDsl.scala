package com.rallyhealth.vapors.v2
package dsl

import algebra.Expr
import logical.Conjunction

trait BuildExprDsl {
  self: DslTypes =>

  extension [V : OP] (value: V) def const: Any ~:> W[V]

  def and[I, O](one: I ~:> W[O], two: I ~:> W[O])(using Conjunction[W[O]], OP[W[O]]): I ~:> W[O] = Expr.And(one, two)
}
