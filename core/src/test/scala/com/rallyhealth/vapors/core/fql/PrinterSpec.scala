package com.rallyhealth.vapors.core.fql

import cats.data.NonEmptyList
import com.rallyhealth.vapors.factfilter.Example._
import com.rallyhealth.vapors.factfilter.data.FactsMatch
import com.rallyhealth.vapors.factfilter.dsl._
import org.scalatest.wordspec.AnyWordSpec

class PrinterSpec extends AnyWordSpec {

  "Printer.serialize" should {

    "print an expression that selects a specific fact type using whereAndFactValue(_ > 30)" in {
      val q = {
        withType(FactTypes.Age)
          .whereAnyFactValue(_ > 30)
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.age))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
      assertResult(
        "_.collect { case f: Fact[Int] => f.exists(_.value where x > 30) }",
      ) {
        printer.serialize(q)
      }
    }

    "print an expression that selects a fact type using whereAnyFact(_.at(_.select(_.value)) > 200))" in {
      val q = {
        withType(FactTypes.WeightMeasurement)
          .whereAnyFact(_.at(_.select(_.value)) > 200)
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.weight))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
      assertResult(
        "_.collect { case f: Fact[Int] => f.exists(_.value where x > 200) }",
      ) {
        printer.serialize(q)
      }
    }

    "print an expression that selects a value using the NamedLens select" in {
      val q = {
        or(
          withType(FactTypes.ProbabilityToUse)
            .whereAnyFactValue { value =>
              value.at(_.select(_.scores).atKey("weightloss")).exists { score =>
                score > 0.5 and score < 0.3
              }
            },
          withType(FactTypes.WeightMeasurement)
            .whereAnyFactValue(_ > 150),
        )
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.weight))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
      assertResult(
        "_.collect {" +
          " case f: Fact[Example.Probs] =>" +
          " f.exists(_.value.scores['weightloss'] .exists(_ where x > 0.5 and  where x < 0.3)) " +
          "} or .collect {" +
          " case f: Fact[Int] => f.exists(_.value where x > 150) " +
          "}",
      ) {
        printer.serialize(q)
      }
    }
  }

}
