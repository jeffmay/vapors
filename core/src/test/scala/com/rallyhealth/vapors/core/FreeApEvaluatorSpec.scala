package com.rallyhealth.vapors.core

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.Example._
import com.rallyhealth.vapors.core.data.{Fact, FactsMatch, NoFactsMatch}
import org.scalatest.wordspec.AnyWordSpec

// TODO: Split out logical tests from fact based tests
final class FreeApEvaluatorSpec extends AnyWordSpec {
  import dsl._
  import evaluator._

  private val evaluatorName = evaluator.getClass.getName.dropRight(1).split('.').takeRight(2).mkString(".")

  private def True[T]: Fact[T] => Boolean = _ => true

  private def False[T]: Fact[T] => Boolean = _ => false

  "dsl.query" should {

    "improve type inference by filtering to the required fact type" in {
      val program = query {
        filter[Int](True)
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(NonEmptyList.of(JoeSchmoe.age, JoeSchmoe.weight))))
    }
  }

  // Fact specific tests

  evaluatorName should {

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

  // Logical eDSL tests

  it when {
    "evaluating logical combinators" should {

      "return true for one true in an or" in {
        val program = query {
          or(filter(True))
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(FactsMatch(JoeSchmoe.facts)))
      }
    }

    "return true for one true and one false in an or" in {
      val program = query {
        or(filter(True), filter(False))
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(JoeSchmoe.facts)))
    }

    "return true for a complex or" in {
      val program = query {
        or(filter(False), filter(False), filter(True), filter(False))
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(JoeSchmoe.facts)))
    }

    "return false for a long or" in {
      val program = query {
        or(filter(False), filter(False), filter(False), filter(False))
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return true for one true in an and" in {
      val program = query {
        and(filter(True))
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(JoeSchmoe.facts)))
    }

    "return false for a true and false in an and" in {
      val program = query {
        and(filter(True), filter(False))
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return true for two trues in an and" in {
      val program = query {
        and(filter(True), filter(True))
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(JoeSchmoe.facts)))
    }

    "return false for a complex and" in {
      val program = query {
        and(filter(True), filter(True), filter(False), filter(True))
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return true for a long and" in {
      val program = query {
        and(filter(True), filter(True), filter(True), filter(True))
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(JoeSchmoe.facts)))
    }

    "return true for nested true 'or's in an and" in {
      val program = query {
        and(
          or(filter(True)),
          or(filter(True)),
        )
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(JoeSchmoe.facts)))
    }

    "return false for a nested false or in an and" in {
      val program = query {
        and(
          or(filter(True)),
          or(filter(False)),
        )
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return false for a nested false or in a long and" in {
      val program = query {
        and(
          or(filter(True)),
          or(filter(True)),
          or(filter(False)),
          or(filter(True)),
        )
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return true for a nested false and in an or" in {
      val program = query {
        or(
          and(filter(False)),
          and(filter(False)),
          and(filter(True)),
          and(filter(False)),
        )
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(JoeSchmoe.facts)))
    }

    "return true for a complex structure" in {
      val program = query {
        or(
          and(filter(False), filter(True)),
          and(filter(False)),
          and(filter(False)),
          or(
            and(
              filter(True),
              filter(False),
            ),
            filter(True),
          ),
        )
      }
      val result = evalQuery(JoeSchmoe.facts.toList)(program)
      assert(result.contains(FactsMatch(JoeSchmoe.facts)))
    }
  }

//  "combine matching facts from and operator" in {
  // val result = evaluate(
  //   Facts.all,
  //   has(Facts.age) && has(Facts.probs)
  // )
  // assertResult(List(Facts.age, Facts.probs)) {
  //   result.matchingFacts
  // }
//  }
}
