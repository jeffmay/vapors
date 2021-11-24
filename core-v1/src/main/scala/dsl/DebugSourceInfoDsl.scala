package com.rallyhealth.vapors.v1

package dsl

import debug.HasDebugSourceInfo

trait DebugSourceInfoDsl {
  self: DslTypes =>

  override final type OP[V] = HasDebugSourceInfo[V]
}
