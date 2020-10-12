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
          __.withType(FactTypes.Age)
            .whereAnyFactValue(_ > 40),
          __.withType(FactTypes.WeightMeasurement)
            .whereAnyFactValue(_ > 300),
          __.withType(FactTypes.ProbabilityToUse)
            .whereAnyFactValue { value =>
              value.at(_.select(_.scores).atKey("weightloss")).exists {
                _ > 0.5
              }
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
            __.withTypeIn(FactTypeSets.Weight)
              .whereAnyFactValue { value =>
                value > 200 and value <= 300
              },
            __.withTypeIn(FactTypeSet.of(FactTypes.Age))
              .whereAnyFactValue {
                _ > 45
              },
          ),
          __.withTypeIn(FactTypeSets.Weight)
            .whereAnyFactValue {
              _ > 300
            },
          __.withType(FactTypes.ProbabilityToUse)
            .whereAnyFactValue { value =>
              value.at(_.select(_.scores).atKey("weightloss")).exists {
                _ > 0.5
              }
            },
        )
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact has prob for weightloss > existing amount" in {
      val q = {
        __.withType(FactTypes.ProbabilityToUse)
          .whereAnyFactValue { value =>
            value.at(_.select(_.scores).atKey("weightloss")).exists {
              _ > 0.4
            }
          }
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "return matching facts if any fact has prob for string === value" in {
      val q = {
        __.withType(FactTypes.Tag)
          .whereAnyFactValue {
            _ === "asthma"
          }
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.asthmaTag))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }

    "using whereEveryValue" in {
      val q = {
        __.withType(FactTypes.BloodPressureMeasurement)
          .whereEveryFactValue { value =>
            and(
              value.at(_.select(_.diastolic)) < 150,
              value.at(_.select(_.systolic)) < 100,
            )
          }
      }
      assertResult(FactsMatch(NonEmptyList.of(JoeSchmoe.bloodPressure))) {
        evalWithFacts(JoeSchmoe.facts)(q)
      }
    }
  }

}
