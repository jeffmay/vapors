package com.rallyhealth.vapors.v1

package dsl

import data.Justified

trait JustifiedExprDsl extends Any with DslTypes {
  override final type W[+O] = Justified[O]
}
