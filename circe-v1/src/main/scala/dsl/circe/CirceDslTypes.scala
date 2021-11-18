package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.DslTypes

trait CirceDslTypes extends Any with DslTypes {
  type OP[a] <: HasEncoder[a]
}
