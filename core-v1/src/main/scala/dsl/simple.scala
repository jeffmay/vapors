package com.rallyhealth.vapors.v1

package dsl

import debug.HasSourceCodeInfo

object simple extends BuildIdExprDsl with SimpleRunDsl {

  override type OP[_] = NoOP

  final object withSourceInfo extends BuildIdExprDsl with SimpleRunDsl {

    override type OP[_] = HasSourceCodeInfo
  }

  final object justified extends JustifiedBuildExprDsl with SimpleRunDsl {

    override type OP[_] = NoOP

    final object withSourceInfo extends JustifiedBuildExprDsl with SimpleRunDsl {

      override type OP[_] = HasSourceCodeInfo
    }
  }
}
