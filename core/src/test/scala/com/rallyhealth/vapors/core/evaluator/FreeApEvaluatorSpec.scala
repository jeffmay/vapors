package com.rallyhealth.vapors.core.evaluator

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.Example._
import com.rallyhealth.vapors.core.data.FactsMatch
import com.rallyhealth.vapors.core.dsl
import org.scalatest.wordspec.AnyWordSpec

// TODO: Split out logical tests from fact based tests
final class FreeApEvaluatorSpec extends AnyWordSpec {
  import dsl._

  "dsl.query" should {

    "improve type inference by filtering to the required fact type" in {
      val program = query {
        filter[Int](_ => true)
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(NonEmptyList.of(JoeSchmoe.age, JoeSchmoe.weight))))
    }
  }

  // Fact specific tests

  "evaluator" should {

    "filter to the expected fact types in nested logical steps" in {
      val exp = query {
        or(
          // TODO: Clean up the redundancy of specifying age twice by using a more powerful CondExpBuilder
          __.withFactsOfType(FactTypes.age) {
            __.whereAnyFactHas(__.factTypeOf(FactTypes.age).whereValue(__ > 300))
          },
          __.withFactsOfType(FactTypes.weight) {
            __.whereAnyFactHas(__.factTypeOf(FactTypes.weight).whereValue(__ > 300))
          },
          __.withFactsOfType(FactTypes.probs) {
            __.whereAnyFactHas {
              __.factTypeOf(FactTypes.probs)
                .whereValueAt(_.select(_.scores).atKey("weightloss")) {
                  __.exists(__ > 0.5)
                }
            }
          },
        )
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(exp)
      assert(result.contains(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))))
    }

    "return matching facts if any fact has prob for weightloss > existing amount" in {
      val exp = query {
        __.whereAnyFactHas {
          __.factTypeOf(FactTypes.probs)
            .whereValueAt(_.select(_.scores).atKey("weightloss")) {
              exists(__ > 0.4)
            }
        }
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(exp)
      assert(result.contains(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))))
    }
  }

}
