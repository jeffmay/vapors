package com.rallyhealth.vapors.core.example

import cats.Order
import com.rallyhealth.vapors.core.data.TimeOrder.LatestFirst._
import com.rallyhealth.vapors.core.data._

import java.time.Instant

final case class Probs(scores: Map[String, Double])

object Probs {
  implicit val order: Order[Probs] = Order.whenEqual(
    Order.reverse(Order.by(_.scores.size)),
    Order.by(_.scores.toSeq),
  )
}

sealed trait Role

object Role {
  case object Admin extends Role
  case object User extends Role

  implicit val order: Order[Role] = Order.reverse {
    Order.by[Role, Int] {
      case Admin => 1
      case User => 2
    }
  }
}

sealed trait ColorCoding

object ColorCoding {
  final case object Red extends ColorCoding
  final case object Green extends ColorCoding
  final case object Blue extends ColorCoding
}

trait HasTimestamp {
  def timestamp: Instant
}

object HasTimestamp {
  implicit val extractTimestamp: ExtractInstant[HasTimestamp] = _.timestamp
}

sealed trait Measurement extends HasTimestamp {
  def name: String
}

object Measurement {
  implicit def orderByLatestTimestamp[M <: Measurement]: Order[M] = Order.by(_.timestamp)
}

sealed trait NumericMeasurement extends Measurement {
  def value: Double
  def unit: String
}

final case class BloodPressure(
  diastolic: Double,
  systolic: Double,
  timestamp: Instant,
) extends Measurement {
  override def name: String = "blood_pressure"
}

final case class WeightMeasurementLbs(
  value: Double,
  timestamp: Instant,
) extends NumericMeasurement {
  override def name: String = "weight"
  override def unit: String = "lbs"
}

final case class GenericMeasurement(
  name: String,
  value: Double,
  unit: String,
  timestamp: Instant,
) extends Measurement

final case class TagsUpdate(
  tags: Set[String],
  timestamp: Instant,
) extends HasTimestamp

object TagsUpdate {
  implicit val orderByLatestTimestamp: Order[TagsUpdate] = Order.by(_.timestamp)
}
