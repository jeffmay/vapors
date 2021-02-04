package com.rallyhealth.vapors.core.example

import com.rallyhealth.vapors.core.data.{FactType, FactTypeSet}

import java.time.LocalDate

object FactTypes {

  // Order all time-based facts by latest first
  import com.rallyhealth.vapors.core.data.TimeOrder.LatestFirst._

  val Name = FactType[String]("name")
  val Age = FactType[Int]("age")
  val Role = FactType[Role]("role")
  val BirthYear = FactType[Int]("year_of_birth")
  val DateOfBirth = FactType[LocalDate]("date_of_birth")
  val GenericMeasurement = FactType[GenericMeasurement]("generic_measurement")
  val WeightMeasurement = FactType[WeightMeasurementLbs]("weight_measurement")
  val WeightSelfReported = FactType[WeightMeasurementLbs]("weight_self_reported")
  val BloodPressureMeasurement = FactType[BloodPressure]("blood_pressure")
  val Tag = FactType[String]("tag")
  val TagsUpdate = FactType[TagsUpdate]("tags_update")
  val ProbabilityToUse = FactType[Probs]("probability_to_use")
}

object FactTypeSets {
  import FactTypes._
  val Weight = FactTypeSet.of(WeightMeasurement, WeightSelfReported)
}
