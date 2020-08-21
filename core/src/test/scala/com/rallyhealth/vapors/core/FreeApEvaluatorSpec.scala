package com.rallyhealth.vapors.core

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.data.{Fact, FactType, FactsMatch, NoFactsMatch}
import org.scalatest.wordspec.AnyWordSpec

// TODO: Split out logical tests from fact based tests
final class FreeApEvaluatorSpec extends AnyWordSpec {
  import FreeApEvaluatorSpec._
  import dsl._
  import evaluator._

//  private val it = evaluator.getClass.getName.dropRight(1).split('.').takeRight(2).mkString(".")
  private val evaluatorName = evaluator.getClass.getName.dropRight(1).split('.').takeRight(2).mkString(".")

  private def True[T]: Fact[T] => Boolean = _ => true

  private def False[T]: Fact[T] => Boolean = _ => false

  // TODO: Why not put this in the DSL... it's gonna be ugly somewhere. Might as well tuck it into the API somehow.
  private def v[A](
    head: A,
    tail: A*
  ): NonEmptyList[A] = NonEmptyList.of(head, tail: _*)

  private final object FactTypes {
    val name = FactType[String]("name")
    val age = FactType[Int]("age")
    val weight = FactType[Int]("weight")
    val probs = FactType[Probs]("probability_to_use")
  }

  private final object Facts {
    val name = Fact(FactTypes.name, "Joe Schmoe")
    val age = Fact(FactTypes.age, 32)
    val weight = Fact(FactTypes.weight, 150)
    val probs = Fact(FactTypes.probs, Probs(Map("weightloss" -> .8)))

    val all = NonEmptyList.of(
      name,
      age,
      weight,
      probs
    )
  }

  "dsl.query" should {

    "improve type inference by filtering to the required fact type" in {
      val program = query {
        filter[Int](True)
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(FactsMatch(v(Facts.age, Facts.weight))))
    }
  }

  // Fact specific tests

  evaluatorName should {

    "filter to the expected fact types in nested logical steps" in {
      val exp = query {
        or(
          v(
            withFactType(FactTypes.age) {
              filter(_.value > 70)
            },
            withFactType(FactTypes.weight) {
              filter(_.value > 200)
            },
            withFactType(FactTypes.probs) {
              filter(_.value.scores.getOrElse("weightloss", 0.0) > .5)
            }
          )
        )
      }
      val result = evalQuery(Facts.all.toList)(exp)
      assert(result.contains(FactsMatch(v(Facts.probs))))
    }

    "return matching facts if any fact has prob for weightloss > existing amount" in {
      val exp = query {
        withFactType(FactTypes.probs) {
          anyFact {
            // TODO: Fix type inference here
            withFieldValue[Probs, Option[Double]](_.value.scores.get("weightloss"), "prob_weightloss") {
              exists(greaterThan(0.4))
            }
          }
        }
      }
      val result = evalQuery(Facts.all.toList)(exp)
      assert(result.contains(FactsMatch(v(Facts.probs))))
    }
  }

  // Logical eDSL tests

  it when {
    "evaluating logical combinators" should {

      "return true for one true in an or" in {
        val program = query {
          or(v(filter(True)))
        }
        val result = evalQuery(Facts.all.toList)(program)
        assert(result.contains(FactsMatch(Facts.all)))
      }
    }

    "return true for one true and one false in an or" in {
      val program = query {
        or(v(filter(True), filter(False)))
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(FactsMatch(Facts.all)))
    }

    "return true for a complex or" in {
      val program = query {
        or(v(filter(False), filter(False), filter(True), filter(False)))
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(FactsMatch(Facts.all)))
    }

    "return false for a long or" in {
      val program = query {
        or(v(filter(False), filter(False), filter(False), filter(False)))
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return true for one true in an and" in {
      val program = query {
        and(v(filter(True)))
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(FactsMatch(Facts.all)))
    }

    "return false for a true and false in an and" in {
      val program = query {
        and(v(filter(True), filter(False)))
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return true for two trues in an and" in {
      val program = query {
        and(v(filter(True), filter(True)))
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(FactsMatch(Facts.all)))
    }

    "return false for a complex and" in {
      val program = query {
        and(v(filter(True), filter(True), filter(False), filter(True)))
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return true for a long and" in {
      val program = query {
        and(v(filter(True), filter(True), filter(True), filter(True)))
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(FactsMatch(Facts.all)))
    }

    "return true for nested true 'or's in an and" in {
      val program = query {
        and(
          v(
            or(v(filter(True))),
            or(v(filter(True))),
          )
        )
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(FactsMatch(Facts.all)))
    }

    "return false for a nested false or in an and" in {
      val program = query {
        and(
          v(
            or(v(filter(True))),
            or(v(filter(False))),
          )
        )
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return false for a nested false or in a long and" in {
      val program = query {
        and(
          v(
            or(v(filter(True))),
            or(v(filter(True))),
            or(v(filter(False))),
            or(v(filter(True))),
          )
        )
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(NoFactsMatch()))
    }

    "return true for a nested false and in an or" in {
      val program = query {
        or(
          v(
            and(v(filter(False))),
            and(v(filter(False))),
            and(v(filter(True))),
            and(v(filter(False))),
          )
        )
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(FactsMatch(Facts.all)))
    }

    "return true for a complex structure" in {
      val program = query {
        or(
          v(
            and(v(filter(False), filter(True))),
            and(v(filter(False))),
            and(v(filter(False))),
            or(
              v(
                and(
                  v(
                    filter(True),
                    filter(False)
                  )
                ),
                filter(True)
              )
            )
          )
        )
      }
      val result = evalQuery(Facts.all.toList)(program)
      assert(result.contains(FactsMatch(Facts.all)))
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

object FreeApEvaluatorSpec {

  private final case class Probs(scores: Map[String, Double])

}
