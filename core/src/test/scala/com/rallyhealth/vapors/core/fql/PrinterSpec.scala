package com.rallyhealth.vapors.core.fql

import cats.data.NonEmptyList
import com.rallyhealth.vapors.factfilter.Example._
import com.rallyhealth.vapors.factfilter.data.{FactsMatch, NoFactsMatch}
import com.rallyhealth.vapors.factfilter.dsl._
import org.scalatest.wordspec.AnyWordSpec

class PrinterSpec extends AnyWordSpec {

  "Printer.serialize" should {

    "print an expression that selects a specific fact type using whereValue(_ > 30)" in {
      val q = {
        __.withFactsOfType(FactTypes.Age)
          .whereAnyValue(__ > 30)
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

    "print an expression that selects a fact type using where(factValue(_ > 200))" in {
      val q = {
        __.withFactsOfType(FactTypes.WeightMeasurement)
          .whereAnyValue(__ > 200)
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
          __.withFactsOfType(FactTypes.ProbabilityToUse)
            .withValuesAt(_.select(_.scores).atKey("weightloss"))
            .whereAnyValue {
              and(exists(__ > 0.5), exists(__ < 0.3))
            },
          __.withFactsOfType(FactTypes.WeightMeasurement)
            .whereAnyValue(__ > 150),
        )
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.weight))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
      assertResult(
        // TODO: This query looks wrong...
        //       there is a mismatch between how value selection works within .exists() and conditional expressions
        "_.collect {" +
          " case f: Fact[Example.Probs] =>" +
          " f.exists(_.value.scores['weightloss'] .exists(_where x > 0.5) and .exists(_where x < 0.3)) " +
          "} or .collect {" +
          " case f: Fact[Int] =>" +
          " f.exists(_.value where x > 150) " +
          "}",
      ) {
        printer.serialize(q)
      }
    }
  }

}
