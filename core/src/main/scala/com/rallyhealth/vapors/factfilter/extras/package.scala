package com.rallyhealth.vapors.factfilter

import com.rallyhealth.vapors.core.data

package object extras {

  @deprecated("Use com.rallyhealth.vapors.core.data.ExtractInstant instead.", "0.8.0")
  final type ExtractInstant[-T] = data.ExtractInstant[T]

  @deprecated("Use com.rallyhealth.vapors.core.data.ExtractInstant instead.", "0.8.0")
  final val ExtractInstant = data.ExtractInstant
}
