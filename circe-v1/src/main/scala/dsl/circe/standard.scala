package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.StandardRunDsl

object standard extends CirceBuildExprDsl[HasEncoder] with StandardRunDsl[HasEncoder] {

  final object withSourceInfo
    extends CirceBuildExprDsl[CirceDebuggingContext]
    with StandardRunDsl[CirceDebuggingContext]
}
