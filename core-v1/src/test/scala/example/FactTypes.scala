package com.rallyhealth.vapors.v1

package example

import data.FactType

import cats.Order

import java.time.{Clock, Instant, LocalDate}
import scala.collection.immutable.SortedSet

object FactTypes extends OrderTimeImplicits {

  final val Age = FactType[Int]("age")
  final val DateOfBirth = FactType[LocalDate]("date_of_birth")
  final val CombinedTags = FactType[CombinedTags]("combined_tags")
  final val GeoLocation = FactType[GeoLocation]("geolocation")
  final val Scores = FactType[Seq[Double]]("scores")
  final val RoleClass = FactType[RoleClass]("role_class")
  final val WeightLbs = FactType[Double]("weight_lbs")
  final val WeightKg = FactType[Double]("weight_kg")
}

trait OrderTimeImplicits {

  implicit val recentFirstLocalDate: Order[LocalDate] = Order.reverse(Order.fromLessThan(_.isBefore(_)))
}

final case class CombinedTags(
  tags: SortedSet[String],
  timestampMillis: Long,
)

object CombinedTags {

  def now(tags: SortedSet[String])(implicit clock: Clock = Clock.systemUTC()): CombinedTags =
    CombinedTags(tags, Instant.now(clock).toEpochMilli)

  implicit val order: Order[CombinedTags] = Order.reverse(Order.by(_.timestampMillis))
}

final case class NestedSelectable(
  value: String,
  opt: Option[NestedSelectable] = None,
  map: Map[String, NestedSelectable] = Map.empty,
  seq: Seq[NestedSelectable] = Seq.empty,
)

object NestedSelectable {

  final val empty = NestedSelectable("empty")

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

sealed trait RoleClass

object RoleClass {
  implicit val order: Order[RoleClass] = Order.by {
    case _: Admin => 1
    case _: Editor => 2
    case _: Author => 3
    case _: Member => 4
    case _: Subscriber => 5
    case _: User => 6
  }
}

sealed class User(val id: String) extends RoleClass
sealed class Subscriber(id: String) extends User(id)
sealed class Member(
  id: String,
  val name: String,
) extends Subscriber(id)
sealed class Author(
  id: String,
  name: String,
  val articles: Int,
) extends Member(id, name)
sealed class Editor(
  id: String,
  name: String,
  articles: Int,
) extends Author(id, name, articles)
sealed class Admin(
  id: String,
  name: String,
  articles: Int,
) extends Editor(id, name, articles)
