package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.core.data.FactTable
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.factfilter.Example.{FactTypes, TagsUpdate}
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class TakeFromOutputSpec extends AnyWordSpec {

  "Expr.TakeFromOutput" when {
    val updateABC = FactTypes.TagsUpdate(TagsUpdate(Set("A", "B", "C"), Instant.now().minusSeconds(60 * 60 * 24)))
    val updateDEF = FactTypes.TagsUpdate(TagsUpdate(Set("D", "E", "F"), Instant.now()))

    "using .take(n) with a positive number" should {

      "return an empty list if the collection is empty" in {
        val q = withFactsOfType(FactTypes.TagsUpdate).where(_.take(1))
        val res = eval(FactTable.empty)(q)
        assert(res.output.value.isEmpty)
      }

      "return the number of elements selected from the start of the list by fact ordering" in {
        val q = withFactsOfType(FactTypes.TagsUpdate).where(_.take(1))
        val res = eval(FactTable(updateABC, updateDEF))(q)
        assertResult(Seq(updateDEF))(res.output.value)
      }

      "return all the elements of the list by fact ordering when the number requested is greater than the size" in {
        val q = withFactsOfType(FactTypes.TagsUpdate).where(_.take(3))
        val res = eval(FactTable(updateABC, updateDEF))(q)
        assertResult(Seq(updateDEF, updateABC))(res.output.value)
      }
    }

    "using .take(n) with a negative number" should {

      "return an empty list if the collection is empty" in {
        val q = withFactsOfType(FactTypes.TagsUpdate).where(_.take(-1))
        val res = eval(FactTable.empty)(q)
        assert(res.output.value.isEmpty)
      }

      "return the number of elements selected from the end of the list by fact ordering" in {
        val q = withFactsOfType(FactTypes.TagsUpdate).where(_.take(-1))
        val res = eval(FactTable(updateABC, updateDEF))(q)
        assertResult(Seq(updateABC))(res.output.value)
      }

      "return all the elements of the list by fact ordering when the number requested is greater than the size" in {
        val q = withFactsOfType(FactTypes.TagsUpdate).where(_.take(-3))
        val res = eval(FactTable(updateABC, updateDEF))(q)
        assertResult(Seq(updateDEF, updateABC))(res.output.value)
      }
    }

    "using .take(n) with 0" should {

      "return an empty collection" in {
        val q = withFactsOfType(FactTypes.TagsUpdate).where(_.take(0))
        val res = eval(FactTable(updateABC, updateDEF))(q)
        assertResult(Seq.empty)(res.output.value)
      }
    }

    "using .headOption" should {

      "return None if the collection is empty" in {
        val q = withFactsOfType(FactTypes.TagsUpdate).where(_.headOption)
        val res = eval(FactTable.empty)(q)
        assertResult(None)(res.output.value)
      }

      "return the head of the collection by fact ordering" in {
        val q = withFactsOfType(FactTypes.TagsUpdate).where(_.headOption)
        val res = eval(FactTable(updateABC, updateDEF))(q)
        assertResult(Some(updateDEF))(res.output.value)
      }
    }
  }
}
