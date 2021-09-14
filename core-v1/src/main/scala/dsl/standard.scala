package com.rallyhealth.vapors.v1

package dsl

import debug.HasSourceCodeInfo

object standard extends BuildExprDsl with StandardRunDsl {

  override type OP[_] = DummyImplicit

  final object withSourceInfo extends BuildExprDsl with StandardRunDsl {

    override type OP[_] = HasSourceCodeInfo
  }
}
