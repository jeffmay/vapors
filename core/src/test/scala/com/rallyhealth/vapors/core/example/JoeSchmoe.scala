package com.rallyhealth.vapors.core.example

import com.rallyhealth.vapors.core.data.{FactSet, FactTable}

import java.time.{LocalDate, ZoneOffset}

object JoeSchmoe {
  val name = FactTypes.Name("Joe Schmoe")
  val age = FactTypes.Age(32)
  val userRole = FactTypes.Role(Role.User)
  val adminRole = FactTypes.Role(Role.Admin)
  val dateOfBirth = FactTypes.DateOfBirth(LocalDate.of(1987, 1, 1))

  val weight = FactTypes.WeightMeasurement(
    WeightMeasurementLbs(250.0, LocalDate.of(2020, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC)),
  )

  val weightSelfReported = FactTypes.WeightSelfReported(
    WeightMeasurementLbs(240.0, LocalDate.of(2019, 1, 30).atStartOfDay().toInstant(ZoneOffset.UTC)),
  )

  val bloodPressure = FactTypes.BloodPressureMeasurement(
    BloodPressure(120, 80, LocalDate.of(2020, 5, 1).atStartOfDay().toInstant(ZoneOffset.UTC)),
  )

  val probs = FactTypes.ProbabilityToUse(Probs(Map("weightloss" -> .8)))
  val asthmaTag = FactTypes.Tag("asthma")

  val facts = FactSet(
    name,
    age,
    adminRole,
    userRole,
    weight,
    weightSelfReported,
    bloodPressure,
    probs,
    asthmaTag,
  )

  val factTable = FactTable(facts.toList)
}
