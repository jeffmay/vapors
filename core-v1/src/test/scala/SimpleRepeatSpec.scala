package com.rallyhealth.vapors.v1

import data.FactTable
import example.FactTypes

import munit.FunSuite
import shapeless.{HNil, Nat}

class SimpleRepeatSpec extends FunSuite {

  import dsl.uncached._

  test("repeat[A].take(10)") {
    // TODO: The following use case should also be supported once .take is supported
    // repeat(1.const).take(10)
  }

  test("wrap(repeat[A], Seq[B]).zipToShortest produces a Seq[A :: B :: HNil]") {
    val ageFacts = valuesOfType(FactTypes.Age)
    val scoreFacts = valuesOfType(FactTypes.Scores)
    val expr = wrap(ageFacts.headOption, scoreFacts.headOption).zipToShortest.map { hl =>
      val age = hl.get(_.at(Nat._0))
      val scores = hl.get(_.at(Nat._1))
      wrap(repeat(age), scores).zipToShortest
    }
    val facts = FactTable(
      FactTypes.Age(30),
      FactTypes.Scores(Seq(1d, 2d)),
    )
    val observed = expr.run(facts)
    val expected = Some(Seq(30 :: 1d :: HNil, 30 :: 2d :: HNil))
    assertEquals(observed, expected)
  }
}
