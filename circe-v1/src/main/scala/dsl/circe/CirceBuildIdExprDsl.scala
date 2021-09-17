package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.BuildIdExprDsl

// TODO: Do JustifedExprDsl
trait CirceBuildIdExprDsl extends BuildIdExprDsl with CirceVaporsEncoders {

  override type OP[a] <: HasEncoder[a]
}
