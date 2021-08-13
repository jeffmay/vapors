package com.rallyhealth

package vapors.interpreter

import vapors.data.{Evidence, FactTable}
import vapors.dsl._
import vapors.example.{FactTypes, JoeSchmoe}

import org.scalatest.Inside.inside
import org.scalatest.freespec.AnyFreeSpec

import java.time.Instant

class ConcatOutputSpec extends AnyFreeSpec {

  "Expr.ConcatOutput" - {

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

    "combine all elements into the given traversable type" in {
      val factTypes = List(FactTypes.TagsUpdate, FactTypes.WeightMeasurement, FactTypes.WeightSelfReported)
      val expectedFacts = factTypes.flatMap(JoeSchmoe.factTable.getSortedSeq(_))
      val instantQueries = factTypes.map { factType =>
        valuesOfType(factType).map(_.get(_.select(_.timestamp))).returnOutput
      }
      val query: RootExpr[Seq[Instant], Unit] = concat(instantQueries: _*).toOutputMonoid
      val result = engine.eval(query, JoeSchmoe.factTable)
      assertResult(expectedFacts.map(_.value.timestamp)) {
        engine.extract(result.value)
      }
      for (foundEvidence <- result.maybeEvidence) {
        assertResult(Evidence(expectedFacts)) {
          engine.extract(foundEvidence)
        }
      }
    }

    "combine an empty list of expressions into an empty list" in {
      val query = concat[FactTable, List, Nothing, Unit]().toOutputMonoid
      val result = engine.eval(query, JoeSchmoe.factTable)
      assertResult(Nil) {
        engine.extract(result.value)
      }
      for (evidence <- result.maybeEvidence) {
        assertResult(Evidence.none) {
          engine.extract(evidence)
        }
      }
    }

    "concat mixed elements types into a common supertype" in {
      val factTypes = List(FactTypes.TagsUpdate, FactTypes.WeightMeasurement, FactTypes.WeightSelfReported)
      val expectedFacts = factTypes.flatMap(JoeSchmoe.factTable.getSortedSeq(_))
      val instantQueries = factTypes.map(factsOfType(_).returnOutput)
      val query = concat(instantQueries: _*).toOutputMonoid.withOutputFoldable.map(_.value.get(_.select(_.timestamp)))
      val result = engine.eval(query, JoeSchmoe.factTable)
      assertResult(expectedFacts.map(_.value.timestamp)) {
        engine.extract(result.value)
      }
      for (evidence <- result.maybeEvidence) {
        assertResult(Evidence(expectedFacts)) {
          engine.extract(evidence)
        }
      }
    }

    "combine all elements into a LazyList" in {
      val factTypes = List(FactTypes.TagsUpdate, FactTypes.WeightMeasurement, FactTypes.WeightSelfReported)
      val expectedFacts = factTypes.flatMap(JoeSchmoe.factTable.getSortedSeq(_))
      val instantQueries = factTypes.map { factType =>
        valuesOfType(factType).map(_.get(_.select(_.timestamp))).returnOutput
      }
      val query = concat(instantQueries: _*).toLazyList
      val result = engine.eval(query, JoeSchmoe.factTable)
      assertResult(expectedFacts.map(_.value.timestamp)) {
        engine.extract(result.value)
      }
      for (evidence <- result.maybeEvidence) {
        assertResult(Evidence(expectedFacts)) {
          engine.extract(evidence)
        }
      }
    }

    "combined all elements into a LazyList without forcing the values" in {
      val factTypes = List(FactTypes.TagsUpdate, FactTypes.WeightMeasurement, FactTypes.WeightSelfReported)
      val instantQueries = factTypes.map { factType =>
        valuesOfType(factType).map(_.get(_.select(_.timestamp))).returnOutput
      }
      val query = concat(instantQueries: _*).toLazyList
      val result = engine.eval(query, JoeSchmoe.factTable)
      val resultValue = engine.extract(result.value)
      inside(resultValue) {
        case values: LazyList[_] =>
          // confirm that the collection does not start with a definite size because it was unforced
          assert(!values.hasDefiniteSize)
          // confirm that forcing the collection causes it to have a definite size
          assert(values.force.hasDefiniteSize)
      }
    }

    "combine an empty list of expressions into an empty lazy list" in {
      val query = concat[FactTable, List, Nothing, Unit]().toLazyList
      val result = engine.eval(query, JoeSchmoe.factTable)
      assertResult(LazyList.empty) {
        engine.extract(result.value)
      }
      for (evidence <- result.maybeEvidence) {
        assertResult(Evidence.none) {
          engine.extract(evidence)
        }
      }
    }
  }
}
