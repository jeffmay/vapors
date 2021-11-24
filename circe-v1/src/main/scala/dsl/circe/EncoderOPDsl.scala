package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.DslTypes

trait EncoderOPDsl {
  self: DslTypes =>

  override final type OP[V] = HasEncoder[V]
}
