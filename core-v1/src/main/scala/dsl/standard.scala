package com.rallyhealth.vapors.v1

package dsl

import debug.HasSourceCodeInfo

object standard extends BuildIdExprDsl with StandardRunDsl {

  override type OP[_] = NoOP

  final object justified extends JustifiedBuildExprDsl with StandardRunDsl {

    override type OP[_] = NoOP
  }

  final object withSourceInfo extends BuildIdExprDsl with StandardRunDsl {

    override type OP[_] = HasSourceCodeInfo
  }
}
