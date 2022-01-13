package com.rallyhealth.vapors.v1

package dsl
import data.FactTable

trait StatelessRunDsl extends RunExprDsl {
  self: DslTypes =>

  override final type RunState = Unit

  override protected final def initRunState(factTable: FactTable): Unit = ()
}
