package com.rallyhealth.vapors.v1

import data.FactTable
import example.FactTypes

import scala.collection.immutable.SortedSet

class JustifiedCollectionEvidenceSpec extends munit.FunSuite {

  import dsl.simple.justified._

  test(".exists returns all evidence when empty".fail) {
    val emptyTagsFact = FactTypes.CombinedTags(SortedSet())
    val expr = valuesOfType(FactTypes.CombinedTags).exists {
      _.exists {
        _ > "C"
      }
    }
    val result = expr.run(FactTable(emptyTagsFact))
    assert(!result.value)
    // TODO: This should contain the original fact as evidence
    println(result)
    assert(result.evidence.nonEmpty)
  }

  test(".exists returns true") {
    val expr = List(1, 2, 3).const.exists {
      _ > 2
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".exists returns false") {
    val expr = List(1, 2, 3).const.exists {
      _ > 3
    }
    val result = expr.run()
    assert(!result.value)
  }

  test(".forall returns true when empty") {
    val expr = List.empty[Int].const.forall {
      _ > 1
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".forall returns true when non-empty and condition is met") {
    val expr = List(2, 3).const.exists {
      _ > 1
    }
    val result = expr.run()
    assert(result.value)
  }

  test(".forall returns false when non-empty and condition is not met") {
    val expr = List(1, 2, 3).const.forall {
      _ > 1
    }
    val result = expr.run()
    assert(!result.value)
  }
}
