package com.rallyhealth.vapors.core

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.data.{Fact, FactType}

private[core] object Example {

  final case class Probs(scores: Map[String, Double])

  final object FactTypes {
    val name = FactType[String]("name")
    val age = FactType[Int]("age")
    val weight = FactType[Int]("weight")
    val probs = FactType[Probs]("probability_to_use")
  }

  final object JoeSchmoe {
    val name = Fact(FactTypes.name, "Joe Schmoe")
    val age = Fact(FactTypes.age, 32)
    val weight = Fact(FactTypes.weight, 150)
    val probs = Fact(FactTypes.probs, Probs(Map("weightloss" -> .8)))

    val facts = NonEmptyList.of(
      name,
      age,
      weight,
      probs,
    )
  }
}
