package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.{BuildExprDsl, DslTypes, FullDsl, RunExprDsl}

trait CirceDsl extends FullDsl with CirceVaporsEncoders {
  self: DslTypes with BuildExprDsl with RunExprDsl =>

  type OP[a] <: HasEncoder[a]
}
