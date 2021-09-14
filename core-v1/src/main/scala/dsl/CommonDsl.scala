package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

trait CommonDsl[OP[_]] extends Any {

  final type ~>[-I, +O] = Expr[I, O, OP]
}
