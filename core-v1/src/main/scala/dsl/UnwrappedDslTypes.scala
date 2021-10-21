package com.rallyhealth.vapors.v1

package dsl

trait UnwrappedDslTypes extends Any with DslTypes {
  override final type W[+A] = A
}
