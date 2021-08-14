package com.rallyhealth

package vapors.interpreter

import vapors.data.{Evidence, FactTable, FactType}
import vapors.dsl._
import vapors.example.{FactTypes, HasTimestamp, JoeSchmoe}

import org.scalatest.Inside.inside
import org.scalatest.freespec.AnyFreeSpec

class WrapOutputSeqSpec extends AnyFreeSpec {

  "Expr.WrapOutputSeq" - {

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

    "wrapSeq / sequence should" - {

      "wrap a list of constant expressions into an expression of a list of the values" in {
        val query = wrapSeq(
          const(1),
          const(2),
          const(3),
        )
        val result = engine.evalAndExtractValue(query)
        assertResult(Seq(1, 2, 3)) {
          result
        }
      }

      "not force the resulting lazy list" in {
        val query = wrapSeq(
          const(1),
          const(2),
          const(3),
        )
        val result = engine.evalAndExtractValue(query)
        inside(result) {
          case values: LazyList[_] =>
            // confirm that the collection does not start with a definite size because it was unforced
            assert(!values.hasDefiniteSize)
            // confirm that forcing the collection causes it to have a definite size
            assert(values.force.hasDefiniteSize)
        }
      }

      "wrap a list of expressions and combine the evidence from all of them" in {
        val factTypes = List[FactType[_ <: HasTimestamp]](
          FactTypes.BloodPressureMeasurement,
          FactTypes.WeightSelfReported,
          FactTypes.TagsUpdate,
        )
        val subExpressions =
          factTypes.map(t => valuesOfType(t).map(_.get(_.select(_.timestamp))).returnOutput)
        val query = sequence(subExpressions)
        val factsPerType = factTypes.map(JoeSchmoe.factTable.getSortedSeq(_))
        val expectedValues = factsPerType.map(_.map(_.value.timestamp))
        val result = engine.eval(query, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assertResult(expectedValues) {
          resultValue
        }
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(factsPerType.flatten)) {
            engine.extract(evidence)
          }
        }
      }
    }
  }
}
