package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

trait DslTypes extends Any {

  type OP[O]

  final type OPW[O] = OP[W[O]]

  type W[+V]

  final type ~>[-I, +O] = Expr[I, O, OP]
}
