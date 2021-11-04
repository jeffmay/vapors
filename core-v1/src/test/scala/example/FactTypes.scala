package com.rallyhealth.vapors.v1

package example

import data.FactType

import cats.Order

import scala.collection.immutable.SortedSet

object FactTypes {

  final val Age = FactType[Int]("age")
  final val CombinedTags = FactType[CombinedTags]("combined_tags")
  final val Weight = FactType[Int]("weight")
}

final case class CombinedTags(
  tags: SortedSet[String],
  timestampMillis: Long,
)

object CombinedTags {
  implicit val order: Order[CombinedTags] = Order.reverse(Order.by(_.timestampMillis))
}
