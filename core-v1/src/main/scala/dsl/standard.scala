package com.rallyhealth.vapors.v1

package dsl

import debug.HasSourceCodeInfo

object standard extends UnwrappedBuildExprDsl with StandardRunDsl {

  override type OP[_] = NoOP

  final object justified extends JustifiedBuildExprDsl with StandardRunDsl {

    override type OP[_] = NoOP
  }

  final object withSourceInfo extends UnwrappedBuildExprDsl with StandardRunDsl {

    override type OP[_] = HasSourceCodeInfo
  }
}
