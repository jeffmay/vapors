package com.rallyhealth.vapors.v1

package data

import munit.FunSuite

import java.time.LocalDate

class FactTableSpec extends FunSuite {

  import example.FactTypes._

  test("FactTable.iterator removes duplicates") {
    val allFacts = Seq[Fact](Age(23), Age(23), DateOfBirth(LocalDate.now()))
    val factTable = FactTable(allFacts)
    assertEquals(factTable.iterator.toSeq.sorted, allFacts.distinct.sorted)
  }

  test("FactTable.toSet") {
    val allFacts = Seq[Fact](Age(23), DateOfBirth(LocalDate.now()), Age(24))
    val factTable = FactTable(allFacts)
    assertEquals(factTable.iterator.toSet, allFacts.toSet)
  }

  test("FactTable.toSeq") {
    val allFacts = Seq[Fact](Age(23), DateOfBirth(LocalDate.now()), Age(24))
    val factTable = FactTable(allFacts)
    assertEquals(factTable.iterator.toSeq.sorted, allFacts.sorted)
  }
}
