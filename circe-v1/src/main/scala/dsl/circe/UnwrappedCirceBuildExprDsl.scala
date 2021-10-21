package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.UnwrappedBuildExprDsl

// TODO: Do JustifedExprDsl
trait UnwrappedCirceBuildExprDsl extends UnwrappedBuildExprDsl with CirceVaporsEncoders {

  override type OP[a] <: HasEncoder[a]
}
