package com.rallyhealth.vapors.v1

package dsl.circe

import dsl.{JustifiedBuildExprDsl, SimpleRunDsl, UnwrappedBuildExprDsl}

object simple extends CirceDsl with UnwrappedBuildExprDsl with SimpleRunDsl with EncoderOPDsl {

  final object justified extends CirceDsl with JustifiedBuildExprDsl with SimpleRunDsl with EncoderOPDsl {

    final object withSourceInfo extends CirceDsl with JustifiedBuildExprDsl with SimpleRunDsl with CirceDebuggingDsl
  }

  final object withSourceInfo extends CirceDsl with UnwrappedBuildExprDsl with SimpleRunDsl with CirceDebuggingDsl
}
