package com.rallyhealth.vapors.v1

package dsl

import debug.HasSourceCodeInfo

object standard extends BuildIdExprDsl with StandardRunDsl {

  override type OP[_] = DummyImplicit

  final object justified extends BuildJustifiedExprDsl with StandardRunDsl {

    override type OP[_] = DummyImplicit
  }

  final object withSourceInfo extends BuildIdExprDsl with StandardRunDsl {

    override type OP[_] = HasSourceCodeInfo
  }
}
