package com.rallyhealth.vapors.v1

package dsl

trait IdExprDsl extends Any with DslTypes {
  override final type W[+A] = A
}
