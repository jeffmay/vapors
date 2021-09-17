package com.rallyhealth.vapors.v1

package example

import data.FactType

object FactTypes {

  final val Age = FactType[Int]("age")
  final val Weight = FactType[Int]("weight")
}
