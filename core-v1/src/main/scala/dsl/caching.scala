package com.rallyhealth.vapors.v1

package dsl

case object caching {

  case object immutable extends FullDsl with UnwrappedBuildExprDsl with ImmutableCachingRunDsl with ShowOPDsl {

    case object withDebugInfo extends UnwrappedBuildExprDsl with ImmutableCachingRunDsl with DebugSourceInfoDsl

    case object justified extends FullDsl with JustifiedBuildExprDsl with ImmutableCachingRunDsl with ShowOPDsl {

      case object withDebugInfo
        extends FullDsl
        with JustifiedBuildExprDsl
        with ImmutableCachingRunDsl
        with DebugSourceInfoDsl
    }
  }
}
