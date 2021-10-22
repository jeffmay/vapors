package com.rallyhealth.vapors.v1

package dsl

import debug.HasSourceCodeInfo

case object standard extends UnwrappedBuildExprDsl with StandardRunDsl {

  override type OP[_] = NoOP

  final case object justified extends JustifiedBuildExprDsl with StandardRunDsl {

    override type OP[_] = NoOP
  }

  final case object withSourceInfo extends UnwrappedBuildExprDsl with StandardRunDsl {

    override type OP[_] = HasSourceCodeInfo
  }
}
