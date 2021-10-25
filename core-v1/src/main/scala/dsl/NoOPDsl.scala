package com.rallyhealth.vapors.v1

package dsl

trait NoOPDsl {
  self: DslTypes =>

  override final type OP[O] = NoOP
}
