package com.rallyhealth.vapors.v1

package example

import data.FactType

import cats.Order

import java.time.Instant
import scala.collection.immutable.SortedSet

object FactTypes {

  final val Age = FactType[Int]("age")
  final val CombinedTags = FactType[CombinedTags]("combined_tags")
  final val GeoLocation = FactType[GeoLocation]("geolocation")
  final val Weight = FactType[Int]("weight")
}

final case class CombinedTags(
  tags: SortedSet[String],
  timestampMillis: Long,
)

object CombinedTags {

  // TODO: Take a clock for testing?
  def now(tags: SortedSet[String]): CombinedTags = CombinedTags(tags, Instant.now().toEpochMilli)

  implicit val order: Order[CombinedTags] = Order.reverse(Order.by(_.timestampMillis))
}

final case class NestedSelectable(
  value: String,
  opt: Option[NestedSelectable] = None,
  map: Map[String, NestedSelectable] = Map.empty,
  seq: Seq[NestedSelectable] = Seq.empty,
)

object NestedSelectable {
  implicit val order: Order[NestedSelectable] = Order.by(_.value)
}

final case class GeoLocation(
  lat: Double,
  lng: Double,
)

object GeoLocation {
  // Order by latitude then longitude
  implicit val order: Order[GeoLocation] = Order.whenEqual(Order.by(_.lat), Order.by(_.lng))
}
