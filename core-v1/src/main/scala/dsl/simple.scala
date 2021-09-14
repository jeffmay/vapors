package com.rallyhealth.vapors.v1

package dsl

import debug.HasSourceCodeInfo

object simple extends BuildExprDsl with SimpleRunDsl {

  override type OP[_] = DummyImplicit

  final object withSourceInfo extends BuildExprDsl with SimpleRunDsl {

    override type OP[_] = HasSourceCodeInfo
  }
}
