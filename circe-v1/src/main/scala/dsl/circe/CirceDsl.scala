package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.{BuildExprDsl, FullDsl, RunExprDsl}

trait CirceDsl extends FullDsl with CirceDslTypes with CirceVaporsEncoders {
  self: BuildExprDsl with RunExprDsl =>
}
