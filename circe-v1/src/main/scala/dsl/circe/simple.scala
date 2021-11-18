package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.{JustifiedBuildExprDsl, SimpleRunDsl, UnwrappedBuildExprDsl}

object simple extends CirceDsl with UnwrappedBuildExprDsl with SimpleRunDsl {

  override type OP[a] = HasEncoder[a]

  final object justified extends CirceDsl with JustifiedBuildExprDsl with SimpleRunDsl {
    override type OP[a] = HasEncoder[a]

    final object withSourceInfo extends CirceDsl with JustifiedBuildExprDsl with SimpleRunDsl {
      override type OP[a] = CirceDebuggingContext[a]
    }
  }

  final object withSourceInfo extends CirceDsl with UnwrappedBuildExprDsl with SimpleRunDsl {
    override type OP[a] = CirceDebuggingContext[a]
  }
}
