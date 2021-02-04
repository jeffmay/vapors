package com.rallyhealth.vapors.factfilter

import com.rallyhealth.vapors.core.algebra
import com.rallyhealth.vapors.core.dsl

package object dsl {

  @deprecated("Use com.rallyhealth.vapors.core.algebra.CaptureP instead.", "0.8.0")
  final type CaptureP[F[_], V, R, P] = algebra.CaptureP[F, V, R, P]

  @deprecated("Use com.rallyhealth.vapors.core.algebra.CaptureP instead.", "0.8.0")
  final val CaptureP = algebra.CaptureP

  @deprecated("Use com.rallyhealth.vapors.core.dsl.ExprBuilder instead.", "0.8.0")
  final type ExprBuilder[F[_], V, M[_], U, P] = dsl.ExprBuilder[F, V, M, U, P]

  @deprecated("Use com.rallyhealth.vapors.core.dsl.ExprBuilder instead.", "0.8.0")
  final val ExprBuilder = dsl.ExprBuilder

  @deprecated("Use com.rallyhealth.vapors.core.dsl.ExprBuilderCatsInstances instead.", "0.8.0")
  final type ExprBuilderCatsInstances = dsl.ExprBuilderCatsInstances

  @deprecated("Import from com.rallyhealth.vapors.core.dsl._ instead.", "0.8.0")
  final val ExprDsl = dsl.ExprDsl

  @deprecated("Use com.rallyhealth.vapors.core.dsl.WrapExprSyntax instead.", "0.8.0")
  final type WrapExprSyntax = dsl.WrapExprSyntax
}
