package com.rallyhealth.vapors.v1

package dsl

import debug.HasSourceCodeInfo

case object simple extends UnwrappedBuildExprDsl with SimpleRunDsl {

  override type OP[_] = NoOP

  final case object withSourceInfo extends UnwrappedBuildExprDsl with SimpleRunDsl {

    override type OP[_] = HasSourceCodeInfo
  }

  final case object justified extends JustifiedBuildExprDsl with SimpleRunDsl {

    override type OP[_] = NoOP

    final case object withSourceInfo extends JustifiedBuildExprDsl with SimpleRunDsl {

      override type OP[_] = HasSourceCodeInfo
    }
  }
}
