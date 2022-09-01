package com.rallyhealth.vapors.v1
package dsl.dynamic

import dsl.{BuildExprDsl, DslTypes, FullDsl, RunExprDsl, UnwrappedDslTypes}

import com.rallyhealth.vapors.v1.algebra.Expr

trait DynamicUnwrappedDsl {

  type AnyExpr = Expr[]
}
