package com.rallyhealth.vapors.v1

package dsl

case object caching {

  final case object immutable extends FullDsl with UnwrappedBuildExprDsl with ImmutableCachingRunDsl with ShowOPDsl {

    final case object withDebugInfo extends UnwrappedBuildExprDsl with ImmutableCachingRunDsl with DebugSourceInfoDsl

    final case object justified extends FullDsl with JustifiedBuildExprDsl with ImmutableCachingRunDsl with ShowOPDsl {

      final case object withDebugInfo
        extends FullDsl
        with JustifiedBuildExprDsl
        with ImmutableCachingRunDsl
        with DebugSourceInfoDsl
    }
  }
}
