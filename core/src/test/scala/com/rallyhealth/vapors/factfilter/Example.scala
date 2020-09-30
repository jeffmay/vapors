package com.rallyhealth.vapors.factfilter

import java.time.LocalDate

import cats.data.NonEmptyList
import com.rallyhealth.vapors.factfilter.data.{FactType, FactTypeSet}

object Example {

  final case class BloodPressure(
    diastolic: Double,
    systolic: Double,
  )

  final case class Probs(scores: Map[String, Double])

  final object FactTypes {
    val Name = FactType[String]("name")
    val Age = FactType[Int]("age")
    val DateOfBirth = FactType[LocalDate]("date_of_birth")
    val WeightMeasurement = FactType[Int]("weight_measurement")
    val WeightSelfReported = FactType[Int]("weight_self_reported")
    val BloodPressureMeasurement = FactType[BloodPressure]("blood_pressure")
    val Tag = FactType[String]("tag")
    val ProbabilityToUse = FactType[Probs]("probability_to_use")
  }

  final object FactTypeSets {
    import FactTypes._
    val Weight = FactTypeSet.of(WeightMeasurement, WeightSelfReported)
  }

  final object JoeSchmoe {
    val name = FactTypes.Name("Joe Schmoe")
    val age = FactTypes.Age(32)
    val dateOfBirth = FactTypes.DateOfBirth(LocalDate.of(1987, 1, 1))
    val weight = FactTypes.WeightMeasurement(250)
    val weightSelfReported = FactTypes.WeightSelfReported(200)
    val bloodPressure = FactTypes.BloodPressureMeasurement(BloodPressure(120, 80))
    val probs = FactTypes.ProbabilityToUse(Probs(Map("weightloss" -> .8)))
    val asthmaTag = FactTypes.Tag("asthma")

    val facts = NonEmptyList.of(
      name,
      age,
      weight,
      weightSelfReported,
      bloodPressure,
      probs,
      asthmaTag,
    )
  }
}
