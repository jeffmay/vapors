package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

trait CommonDsl extends Any {

  type OP[_]

  final type ~>[-I, +O] = Expr[I, O, OP]
}
