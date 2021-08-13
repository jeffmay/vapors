package com.rallyhealth

package vapors.interpreter

import vapors.data._
import vapors.dsl._
import vapors.example.{FactTypes, JoeSchmoe}

import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec

class EmbedExprSpec extends AnyFreeSpec {

  "embedding an expression inside a logical operator inside a 'withFactsOfType'" - {

    "standard engine" - {
      allTests(StandardVaporsEngine)
    }

    "cats effect engine" - {
      import cats.effect.unsafe.implicits.global
      allTests(CatsEffectSimpleVaporsEngine)
    }
  }

  def allTests[F[_]](engine: VaporsEngine[F, Unit])(implicit engineExtractParam: engine.ExtractParam): Unit = {

    def insideProbOfWeightloss(cond: ValCondExpr[Double, Unit]): RootExpr[Boolean, Unit] = {
      valuesOfType(FactTypes.ProbabilityToUse)
        .flatMap {
          _.getFoldable {
            _.select(_.scores).at("weightloss").to(Seq)
          }
        }
        .exists { _ =>
          cond
        }
    }

    def weightMeasuredWithin(window: Window[Int]): RootExpr[Boolean, Unit] = {
      import cats.syntax.invariant._
      val doubleWindow = window.imap(_.toDouble)(_.toInt)
      valuesOfType(FactTypes.WeightMeasurement).exists {
        _.get(_.select(_.value)).within(doubleWindow)
      }
    }

    val trueEmbedded = weightMeasuredWithin(Window.greaterThan(200))
    val falseEmbedded = weightMeasuredWithin(Window.greaterThan(300))
    val embeddedFacts = FactSet(JoeSchmoe.weight)

    def trueLiteral: ValCondExpr[Double, Unit] = {
      within(input, const(Window.greaterThan(0.7)))
    }

    def falseLiteral: ValCondExpr[Double, Unit] = {
      within(input, const(Window.greaterThan(0.9)))
    }

    val literalFacts = FactSet(JoeSchmoe.probs)

    "inside an 'or'" - {

      "return 'true' when embedding a 'true' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(or(trueEmbedded, trueLiteral))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(embeddedFacts | literalFacts)) {
            engine.extract(evidence)
          }
        }
      }

      "return 'true' when embedding a 'true' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(or(trueEmbedded, falseLiteral))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(embeddedFacts)) {
            engine.extract(evidence)
          }
        }
      }

      "return 'true' when embedding a 'false' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(or(falseEmbedded, trueLiteral))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(literalFacts)) {
            engine.extract(evidence)
          }
        }
      }

      "return 'false' when embedding a 'false' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(or(falseEmbedded, falseLiteral))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(!resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      }

      "return 'true' when embedding a 'true' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(or(trueLiteral, trueEmbedded))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(literalFacts | embeddedFacts)) {
            engine.extract(evidence)
          }
        }
      }

      "return 'true' when embedding a 'true' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(or(falseLiteral, trueEmbedded))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(embeddedFacts)) {
            engine.extract(evidence)
          }
        }
      }

      "return 'true' when embedding a 'false' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(or(trueLiteral, falseEmbedded))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(literalFacts)) {
            engine.extract(evidence)
          }
        }
      }

      "return 'false' when embedding a 'false' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(or(falseLiteral, falseEmbedded))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(!resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      }

      "disallows embedding an invalid return type" in {
        val listOfNumberExpr = valuesOfType(FactTypes.ProbabilityToUse).flatMap {
          _.getFoldable(_.select(_.scores).at("weightloss").to(Seq))
        }
        assertDoesNotCompile {
          """insideProbOfWeightloss(or(trueLiteral, listOfNumberExpr))"""
        }
      }
    }

    "inside an 'and'" - {

      "return 'true' when embedding a 'true' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(and(trueEmbedded, trueLiteral))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(embeddedFacts | literalFacts)) {
            engine.extract(evidence)
          }
        }
      }

      "return 'false' when embedding a 'true' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(and(trueEmbedded, falseLiteral))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(!resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      }

      "return 'false' when embedding a 'false' expression BEFORE a 'true' literal" in {
        val q = insideProbOfWeightloss(and(falseEmbedded, trueLiteral))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(!resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      }

      "return 'false' when embedding a 'false' expression BEFORE a 'false' literal" in {
        val q = insideProbOfWeightloss(and(falseEmbedded, falseLiteral))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(!resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      }

      "return 'true' when embedding a 'true' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(and(trueLiteral, trueEmbedded))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(literalFacts | embeddedFacts)) {
            engine.extract(evidence)
          }
        }
      }

      "return 'false' when embedding a 'true' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(and(falseLiteral, trueEmbedded))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(!resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      }

      "return 'false' when embedding a 'false' expression AFTER a 'true' literal" in {
        val q = insideProbOfWeightloss(and(trueLiteral, falseEmbedded))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(!resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      }

      "return 'false' when embedding a 'false' expression AFTER a 'false' literal" in {
        val q = insideProbOfWeightloss(and(falseLiteral, falseEmbedded))
        val result = engine.eval(q, JoeSchmoe.factTable)
        val resultValue = engine.extract(result.value)
        assert(!resultValue)
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      }

      "disallows embedding an invalid return type" in {
        val listOfNumberExpr = valuesOfType(FactTypes.ProbabilityToUse).flatMap {
          _.getFoldable(_.select(_.scores).at("weightloss").asIterable.to(Seq))
        }
        assertDoesNotCompile {
          """insideProbOfWeightloss(and(trueLiteral, listOfNumberExpr))"""
        }
      }
    }
  }
}
