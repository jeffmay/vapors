package com.rallyhealth.vapors.core.fql

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.Example._
import com.rallyhealth.vapors.core.data.FactsMatch
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.evaluator._
import org.scalatest.wordspec.AnyWordSpec

class PrinterSpec extends AnyWordSpec {

  "Printer.serialize" should {

    "print an expression that selects a specific fact type using whereValue(_ > 30)" in {
      val q = queryAny {
        __.withFactsOfType(FactTypes.age) {
          __.whereAnyFactHas {
            __.factTypeOf(FactTypes.age).whereValue(__ > 30)
          }
        }
      }
      val results = evalQuery(JoeSchmoe.facts.toList)(q)
      assertResult(Some(FactsMatch(NonEmptyList.of(JoeSchmoe.age))))(results)
      assertResult(
        // TODO: This query is overly redundant, we should not need to match on the type twice
        "match { case Int => exists (match { case Int => _.value where x > 30) }) }",
      ) {
        printer.serialize(q.expression)
      }
    }

    "print an expression that selects a fact type using where(factValue(_ > 200))" in {
      val q = queryAny {
        __.withFactsOfType(FactTypes.weight) {
          __.whereAnyFactHas {
            __.factTypeOf(FactTypes.weight).where(__.factValue(__ > 200))
          }
        }
      }
      val results = evalQuery(JoeSchmoe.facts.toList)(q)
      assertResult(Some(FactsMatch(NonEmptyList.of(JoeSchmoe.weight))))(results)
      assertResult(
        // TODO: This query is overly redundant, we should not need to match on the type twice
        "match { case Int => exists (match { case Int => _.value where x > 200) }) }",
      ) {
        printer.serialize(q.expression)
      }
    }

    "print an expression that selects a value using the NamedLens select" in {
      val q = queryAny {
        __.withFactsOfType(FactTypes.probs) {
          __.whereAnyFactHas {
            __.factTypeOf(FactTypes.probs).whereValueAt(_.select(_.scores).atKey("weightloss")) {
              __.exists(__ > 0.5)
            }
          }
        }
      }
      val results = evalQuery(JoeSchmoe.facts.toList)(q)
      assertResult(Some(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))))(results)
      assertResult(
        // TODO: This query is overly redundant, we should not need to match on the type twice
        "match {" +
          " case com.rallyhealth.vapors.core.Example.Probs => exists (match {" +
          " case com.rallyhealth.vapors.core.Example.Probs => _.value.scores[weightloss] exists (where x > 0.5))" +
          " })" +
          " }",
      ) {
        printer.serialize(q.expression)
      }
    }
  }

}
