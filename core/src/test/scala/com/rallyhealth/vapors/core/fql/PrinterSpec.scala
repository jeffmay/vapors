package com.rallyhealth.vapors.core.fql

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.Example._
import com.rallyhealth.vapors.core.data.FactsMatch
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.evaluator._
import org.scalatest.wordspec.AnyWordSpec

class PrinterSpec extends AnyWordSpec {

  "fql.serialize" should {

    "print correctly" in {
      val q = query {
        withFactType(FactTypes.age) {
          whereAnyFactHas(fieldValue(greaterThan(30)))
        }
      }
      val results = evalQuery(JoeSchmoe.facts.toList)(q)
      assertResult(Some(FactsMatch(NonEmptyList.of(JoeSchmoe.age))))(results)
      assertResult("match { case Int => exists (_.value where x > 30)) }") {
        printer.serialize(q.expression)
      }
    }

    "print a subfield" in {
      val q = query {
        withFactType(FactTypes.probs) {
          whereAnyFactHas {
            fieldValueAt(FactTypes.probs.lens.select(_.scores).atKey("weightloss")) {
              exists(greaterThan(0.5))
            }
          }
        }
      }
      val results = evalQuery(JoeSchmoe.facts.toList)(q)
      assertResult(Some(FactsMatch(NonEmptyList.of(JoeSchmoe.probs))))(results)
      assertResult(
        "match { " +
          "case com.rallyhealth.vapors.core.Example.Probs =>" +
          " exists (_.value.scores[weightloss] exists (where x > 0.5))) " +
          "}",
      ) {
        printer.serialize(q.expression)
      }
    }
  }

}
