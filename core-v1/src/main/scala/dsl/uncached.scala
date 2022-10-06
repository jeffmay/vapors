package com.rallyhealth.vapors.v1

package dsl

case object uncached extends FullDsl with UnwrappedBuildExprDsl with SimpleRunDsl with ShowOPDsl {

  case object withDebugInfo extends UnwrappedBuildExprDsl with SimpleRunDsl with DebugSourceInfoDsl

  case object justified extends FullDsl with JustifiedBuildExprDsl with SimpleRunDsl with ShowOPDsl {

    case object withDebugInfo extends FullDsl with JustifiedBuildExprDsl with SimpleRunDsl with DebugSourceInfoDsl
  }
}
