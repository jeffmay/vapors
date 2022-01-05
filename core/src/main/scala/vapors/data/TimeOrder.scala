package com.rallyhealth

package vapors.data

object TimeOrder {

  @deprecated("This is confusingly named, please use com.rallyhealth.vapors.v1.data.YoungestFirst instead", "1.0.0")
  final val LatestFirst = vapors.v1.data.TimeOrder.YoungestFirst

  @deprecated(
    "This was implemented incorrectly, please use com.rallyhealth.vapors.v1.data.OldestFirst for the correct behavior",
    "1.0.0",
  )
  final val EarliestFirst = vapors.v1.data.TimeOrder.YoungestFirst
}
