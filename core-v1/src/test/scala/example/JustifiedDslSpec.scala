package com.rallyhealth.vapors.v1

package example

import data.{Evidence, FactTable, Justified, NoEvidence}

import munit._

class JustifiedDslSpec extends FunSuite {

  import dsl.simple.justified._

  test("adds constants as their own evidence") {
    val result = (1.const + 1.const).run()
    assertEquals(result.evidence, NoEvidence)
    assertEquals(result.configs, Seq())
    assertEquals(result.value, 2)
  }

  test("returns a fact as its own evidence") {
    val ageFact = FactTypes.Age(1)
    val results = valuesOfType(FactTypes.Age).run(FactTable(ageFact))
    val firstResult = results.head
    assertEquals(firstResult.evidence, Evidence(ageFact))
    assertEquals(firstResult.configs, Seq())
    assertEquals(firstResult.value, ageFact.value)
  }

  test("returns all facts in an AND as evidence") {
    val ageFact = FactTypes.Age(1)
    val results = valuesOfType(FactTypes.Age).run(FactTable(ageFact))
    val firstResult = results.head
    assertEquals(firstResult.evidence, Evidence(ageFact))
    assertEquals(firstResult.configs, Seq())
    assertEquals(firstResult.value, ageFact.value)
  }
}
