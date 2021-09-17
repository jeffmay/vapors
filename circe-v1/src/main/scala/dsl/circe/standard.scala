package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.StandardRunDsl

object standard extends CirceBuildIdExprDsl with StandardRunDsl {

  override type OP[a] = HasEncoder[a]

  final object withSourceInfo extends CirceBuildIdExprDsl with StandardRunDsl {
    override type OP[a] = CirceDebuggingContext[a]
  }
}
