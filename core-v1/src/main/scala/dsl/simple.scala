package com.rallyhealth.vapors.v1

package dsl

import debug.HasSourceCodeInfo

case object simple extends FullDsl with UnwrappedBuildExprDsl with SimpleRunDsl with NoOPDsl {

  final case object withSourceInfo extends UnwrappedBuildExprDsl with SimpleRunDsl {

    override type OP[_] = HasSourceCodeInfo
  }

  final case object justified extends FullDsl with JustifiedBuildExprDsl with SimpleRunDsl with NoOPDsl {

    final case object withSourceInfo extends FullDsl with JustifiedBuildExprDsl with SimpleRunDsl {

      override type OP[_] = HasSourceCodeInfo
    }
  }
}
