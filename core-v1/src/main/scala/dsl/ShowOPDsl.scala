package com.rallyhealth.vapors.v1

package dsl

import debug.HasShow

trait ShowOPDsl {
  self: DslTypes =>

  override final type OP[V] = HasShow[V]
}
