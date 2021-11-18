package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.{JustifiedBuildExprDsl, StandardRunDsl, UnwrappedBuildExprDsl}

object standard extends CirceDsl with UnwrappedBuildExprDsl with StandardRunDsl {

  override type OP[a] = HasEncoder[a]

  final object justified extends CirceDsl with JustifiedBuildExprDsl with StandardRunDsl {
    override type OP[a] = HasEncoder[a]

    final object withSourceInfo extends CirceDsl with JustifiedBuildExprDsl with StandardRunDsl {
      override type OP[a] = CirceDebuggingContext[a]
    }
  }

  final object withSourceInfo extends CirceDsl with UnwrappedBuildExprDsl with StandardRunDsl {
    override type OP[a] = CirceDebuggingContext[a]
  }
}
