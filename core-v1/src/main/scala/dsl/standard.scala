package com.rallyhealth.vapors.v1

package dsl

case object standard extends UnwrappedBuildExprDsl with StandardRunDsl with ShowOPDsl {

  final case object justified extends JustifiedBuildExprDsl with StandardRunDsl with ShowOPDsl {

    final case object withSourceInfo extends UnwrappedBuildExprDsl with StandardRunDsl with DebugSourceInfoDsl
  }

  final case object withSourceInfo extends UnwrappedBuildExprDsl with StandardRunDsl with DebugSourceInfoDsl
}
