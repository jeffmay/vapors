package com.rallyhealth.vapors.v1

package example

import data.FactType

import scala.collection.immutable.SortedSet

object FactTypes {

  final val Age = FactType[Int]("age")
  final val CombinedTags = FactType[SortedSet[String]]("combined_tags")
  final val Weight = FactType[Int]("weight")
}
