package com.rallyhealth.vapors.v1

package dsl

case object uncached extends FullDsl with UnwrappedBuildExprDsl with SimpleRunDsl with ShowOPDsl {

  final case object withDebugInfo extends UnwrappedBuildExprDsl with SimpleRunDsl with DebugSourceInfoDsl

  final case object justified extends FullDsl with JustifiedBuildExprDsl with SimpleRunDsl with ShowOPDsl {

    final case object withDebugInfo extends FullDsl with JustifiedBuildExprDsl with SimpleRunDsl with DebugSourceInfoDsl
  }
}
