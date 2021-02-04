package com.rallyhealth.vapors.factfilter

import com.rallyhealth.vapors.core.algebra

package object dsl {

  @deprecated("Use com.rallyhealth.vapors.core.algebra.CaptureP instead.", "0.8.0")
  final type CaptureP[F[_], V, R, P] = algebra.CaptureP[F, V, R, P]

  @deprecated("Use com.rallyhealth.vapors.core.algebra.CaptureP instead.", "0.8.0")
  final val CaptureP = algebra.CaptureP
}
