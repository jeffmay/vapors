package com.rallyhealth.vapors.core.evaluator

import cats.data.NonEmptyList
import cats.instances.string._
import com.rallyhealth.vapors.factfilter.Example._
import com.rallyhealth.vapors.factfilter.data.{FactTypeSet, FactsMatch}
import com.rallyhealth.vapors.factfilter.dsl._
import org.scalatest.wordspec.AnyWordSpec

final class FreeApEvaluatorSpec extends AnyWordSpec {

  "evaluator" should {

    "filter to the expected fact types" in {
      val q = {
        or(
          __.withFactsOfType(FactTypes.Age)
            .whereAnyValue(__ > 40),
          __.withFactsOfType(FactTypes.WeightMeasurement)
            .whereAnyValue(__ > 300),
          __.withFactsOfType(FactTypes.ProbabilityToUse)
            .withValuesAt(_.select(_.scores).atKey("weightloss"))
            .whereAnyValue {
              exists(__ > 0.5)
            },
        )
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "filter to the expected fact types in nested logical steps" in {
      val q =
        or(
          and(
            __.withFactsOfTypeIn(FactTypeSets.Weight)
              .whereAnyValue {
                __ > 200 and __ <= 300
              },
            __.withFactsOfTypeIn(FactTypeSet.of(FactTypes.Age))
              .whereAnyValue {
                __ > 45
              },
          ),
          __.withFactsOfTypeIn(FactTypeSets.Weight)
            .whereAnyValue {
              __ > 300
            },
          __.withFactsOfType(FactTypes.ProbabilityToUse)
            .withValuesAt(_.select(_.scores).atKey("weightloss"))
            .whereAnyValue {
              exists(__ > 0.5)
            },
        )
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact has prob for weightloss > existing amount" in {
      val q = {
        __.withFactsOfType(FactTypes.ProbabilityToUse)
          .withValuesAt(_.select(_.scores).atKey("weightloss"))
          .whereAnyValue {
            exists(__ > 0.4)
          }
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact has prob for string === value" in {
      val q = {
        __.withFactsOfType(FactTypes.Tag)
          .whereAnyValue {
            __ === "asthma"
          }
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.asthmaTag))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }
  }

}
