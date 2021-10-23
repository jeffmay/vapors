package com.rallyhealth.vapors.v1

import dsl.{BuildExprDsl, DslTypes, RunExprDsl}

trait BaseDslSpec {

  val thisDsl: BuildExprDsl with RunExprDsl with DslTypes
}
