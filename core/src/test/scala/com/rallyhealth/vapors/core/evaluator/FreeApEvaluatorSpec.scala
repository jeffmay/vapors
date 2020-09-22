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

    // TODO: Remove this once deprecated method is removed
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
      val exp = queryAny {
        or(
          __.withFactsOfType(FactTypes.age)
            .whereAnyValue(__ > 300),
          __.withFactsOfType(FactTypes.weight)
            .whereAnyValue(__ > 300),
          __.withFactsOfType(FactTypes.probs)
            .withValuesAt(_.select(_.scores).atKey("weightloss"))
            .whereAnyValue {
              __.exists(__ > 0.5)
            },
        )
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(exp)
      assert(result.contains(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))))
    }

    "return matching facts if any fact has prob for weightloss > existing amount" in {
      val exp = queryAny {
        __.withFactsOfType(FactTypes.probs)
          .withValuesAt(_.select(_.scores).atKey("weightloss"))
          .whereAnyValue {
            exists(__ > 0.4)
          }
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(exp)
      assert(result.contains(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))))
    }
  }

}
