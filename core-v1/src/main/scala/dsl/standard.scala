package com.rallyhealth.vapors.v1

package dsl

case object standard extends UnwrappedBuildExprDsl with StandardRunDsl with ShowOPDsl {

  case object justified extends JustifiedBuildExprDsl with StandardRunDsl with ShowOPDsl {

    case object withSourceInfo extends UnwrappedBuildExprDsl with StandardRunDsl with DebugSourceInfoDsl
  }

  case object withSourceInfo extends UnwrappedBuildExprDsl with StandardRunDsl with DebugSourceInfoDsl
}
