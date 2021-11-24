package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.DslTypes

trait CirceDebuggingDsl {
  self: DslTypes =>

  override final type OP[V] = CirceDebuggingContext[V]
}
