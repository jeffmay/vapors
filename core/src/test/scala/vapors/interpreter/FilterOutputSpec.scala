package com.rallyhealth

package vapors.interpreter

import vapors.data.{Evidence, FactTable}
import vapors.dsl._
import vapors.example.{FactTypes, Tags}

import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec

class FilterOutputSpec extends AnyFreeSpec {

  "Expr.FilterOutput" - {

    "standard engine" - {
      allTests(StandardVaporsEngine)
    }

    "cats effect engine" - {
      import cats.effect.unsafe.implicits.global
      allTests(CatsEffectSimpleVaporsEngine)
    }
  }

  private def allTests[F[_]](
    engine: VaporsEngine[F, Unit],
  )(implicit
    engineExtractParam: engine.ExtractParam,
  ): Unit = {

    "using with an 'OutputWithinSet' op" - {
      val tagFacts = FactTable(Tags.smoker, Tags.asthma, Tags.normalBmi)

      "comparing facts" - {

        "return all matching facts from a given subset" in {
          val q = factsOfType(FactTypes.Tag).filter {
            _ in Set(Tags.asthma)
          }
          val res = engine.eval(q, tagFacts)
          val resultValue = engine.extract(res.value)
          resultValue should contain theSameElementsAs Seq(Tags.asthma)
          for (evidence <- res.maybeEvidence) {
            assertResult(Evidence(Tags.asthma)) {
              engine.extract(evidence)
            }
          }
        }

        "return all matching facts from a given superset" in {
          val q = factsOfType(FactTypes.Tag).filter {
            _ in Set(Tags.asthma, Tags.obeseBmi)
          }
          val res = engine.eval(q, tagFacts)
          val resultValue = engine.extract(res.value)
          resultValue should contain theSameElementsAs Seq(Tags.asthma)
          for (evidence <- res.maybeEvidence) {
            assertResult(Evidence(Tags.asthma)) {
              engine.extract(evidence)
            }
          }
        }

        "return an empty list of facts when given a set that contains no common elements" in {
          val q = factsOfType(FactTypes.Tag).filter {
            _ in Set(Tags.obeseBmi)
          }
          val res = engine.eval(q, tagFacts)
          val resultValue = engine.extract(res.value)
          assert(resultValue.isEmpty)
          for (evidence <- res.maybeEvidence) {
            assert(engine.extract(evidence).isEmpty)
          }
        }
      }

      "comparing values" - {

        "return all matching values from a given subset" in {
          val q = valuesOfType(FactTypes.Tag).filter {
            _ in Set(Tags.asthma).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          val resultValue = engine.extract(res.value)
          resultValue should contain theSameElementsAs Seq(Tags.asthma).map(_.value)
        }

        "return the correct evidence for the matching values from a given subset" in {
          val q = valuesOfType(FactTypes.Tag).filter {
            _ in Set(Tags.asthma).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          for (evidence <- res.maybeEvidence) {
            // TODO: Merge this assertion the above unit test when it passes
            pendingUntilFixed {
              assertResult(Evidence(Tags.asthma)) {
                engine.extract(evidence)
              }
            }
          }
        }

        "return the matching values from a given superset" in {
          val q = valuesOfType(FactTypes.Tag).filter {
            _ in Set(Tags.asthma, Tags.obeseBmi).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          val resultValue = engine.extract(res.value)
          resultValue should contain theSameElementsAs Seq(Tags.asthma).map(_.value)
        }

        "return the correct evidence for the matching values from a given superset" in {
          val q = valuesOfType(FactTypes.Tag).filter {
            _ in Set(Tags.asthma, Tags.obeseBmi).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          for (evidence <- res.maybeEvidence) {
            // TODO: Merge this assertion the above unit test when it passes
            pendingUntilFixed {
              assertResult(Evidence(Tags.asthma)) {
                engine.extract(evidence)
              }
            }
          }
        }

        "return an empty list of values when given a set that contains no common elements" in {
          val q = valuesOfType(FactTypes.Tag).filter {
            _ in Set(Tags.obeseBmi).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          val resultValue = engine.extract(res.value)
          assert(resultValue.isEmpty)
          for (evidence <- res.maybeEvidence) {
            assert(engine.extract(evidence).isEmpty)
          }
        }
      }

      "using the 'containsAny' op" - {

        "return 'true' when the fact table contains a superset of the given set" in {
          val q = valuesOfType(FactTypes.Tag).containsAny {
            Set(Tags.asthma).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          val resultValue = engine.extract(res.value)
          assert(resultValue)
        }

        "return the correct evidence for the facts that contain a superset of the given set" in {
          val q = valuesOfType(FactTypes.Tag).containsAny {
            Set(Tags.asthma).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          for (evidence <- res.maybeEvidence) {
            // TODO: Merge this assertion the above unit test when it passes
            pendingUntilFixed {
              assertResult(Evidence(Tags.asthma)) {
                engine.extract(evidence)
              }
            }
          }
        }

        "return 'true' when the fact table contains a subset of the given set" in {
          val q = valuesOfType(FactTypes.Tag).containsAny {
            Set(Tags.asthma, Tags.obeseBmi).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          val resultValue = engine.extract(res.value)
          assert(resultValue)
        }

        "return the correct evidence for the facts that contain a subset of the given set" in {
          val q = valuesOfType(FactTypes.Tag).containsAny {
            Set(Tags.asthma, Tags.obeseBmi).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          for (evidence <- res.maybeEvidence) {
            // TODO: Merge this assertion the above unit test when it passes
            pendingUntilFixed {
              assertResult(Evidence(Tags.asthma)) {
                engine.extract(evidence)
              }
            }
          }
        }

        "return 'false' with no Evidence when the facts do not contain anything in the given set" in {
          val q = valuesOfType(FactTypes.Tag).containsAny {
            Set(Tags.obeseBmi).map(_.value)
          }
          val res = engine.eval(q, tagFacts)
          val resultValue = engine.extract(res.value)
          assert(!resultValue)
          for (evidence <- res.maybeEvidence) {
            assert(engine.extract(evidence).isEmpty)
          }
        }
      }
    }

    "using with an 'OutputWithinRange' operator" - {
      val low = FactTypes.Age(10)
      val middle = FactTypes.Age(18)
      val high = FactTypes.Age(85)
      val numericFacts = FactTable(low, middle, high)

      "return all values that match the condition" in {
        val q = valuesOfType(FactTypes.Age).filter {
          _ >= const(middle.value)
        }
        val res = engine.eval(q, numericFacts)
        val resultValue = engine.extract(res.value)
        resultValue should contain theSameElementsAs Seq(middle, high).map(_.value)
      }

      "return the correct evidence for the matching values from a given subset" in {
        val q = valuesOfType(FactTypes.Age).filter {
          _ >= const(middle.value)
        }
        val res = engine.eval(q, numericFacts)
        for (evidence <- res.maybeEvidence) {
          // TODO: Merge this assertion the above unit test when it passes
          pendingUntilFixed {
            assertResult(Evidence(middle, high)) {
              engine.extract(evidence)
            }
          }
        }
      }

      "return all facts that match the condition" in {
        val q = factsOfType(FactTypes.Age).filter {
          _.get(_.select(_.value)) >= const(middle.value)
        }
        val res = engine.eval(q, numericFacts)
        val resultValue = engine.extract(res.value)
        resultValue should contain theSameElementsAs Seq(middle, high)
        for (evidence <- res.maybeEvidence) {
          assertResult(Evidence(middle, high)) {
            engine.extract(evidence)
          }
        }
      }

      "return an empty list of values when none of the elements meet the condition" in {
        val q = valuesOfType(FactTypes.Age).filter {
          _ > const(high.value)
        }
        val res = engine.eval(q, numericFacts)
        val resultValue = engine.extract(res.value)
        assert(resultValue.isEmpty)
        for (evidence <- res.maybeEvidence) {
          assert(engine.extract(evidence).isEmpty)
        }
      }

      "return an empty list of facts when none meet the condition" in {
        val q = valuesOfType(FactTypes.Age).filter {
          _ > const(high.value)
        }
        val res = engine.eval(q, numericFacts)
        val resultValue = engine.extract(res.value)
        assert(resultValue.isEmpty)
        for (evidence <- res.maybeEvidence) {
          assert(engine.extract(evidence).isEmpty)
        }
      }
    }
  }
}
