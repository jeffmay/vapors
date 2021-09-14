package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.BuildExprDsl

trait CirceBuildExprDsl extends BuildExprDsl with CirceVaporsEncoders {

  override type OP[a] <: HasEncoder[a]
}
