package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl._
import vapors.example.{FactTypes, TagsUpdate}

import org.scalatest.freespec.AnyFreeSpec

import java.time.Instant

class TakeFromOutputSpec extends AnyFreeSpec {

  "Expr.TakeFromOutput" - {

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

    val now = Instant.now()
    val updateABC =
      FactTypes.TagsUpdate(TagsUpdate("ABC", Set("A", "B", "C"), now.minusSeconds(60 * 60 * 24)))
    val updateDEF = FactTypes.TagsUpdate(TagsUpdate("DEF", Set("D", "E", "F"), now))

    "using .take(n) with a positive number" - {

      "return an empty list if the collection is empty" in {
        val q = factsOfType(FactTypes.TagsUpdate).take(1)
        val res = engine.evalAndExtractValue(q)
        assert(res.isEmpty)
      }

      "return the number of elements selected from the start of the list by fact ordering" in {
        val q = factsOfType(FactTypes.TagsUpdate).take(1)
        val res = engine.evalAndExtractValue(q, FactTable(updateABC, updateDEF))
        assertResult(Seq(updateDEF))(res)
      }

      "return all the elements of the list by fact ordering when the number requested is greater than the size" in {
        val q = factsOfType(FactTypes.TagsUpdate).take(3)
        val res = engine.evalAndExtractValue(q, FactTable(updateABC, updateDEF))
        assertResult(Seq(updateDEF, updateABC))(res)
      }
    }

    "using .take(n) with a negative number" - {

      "return an empty list if the collection is empty" in {
        val q = factsOfType(FactTypes.TagsUpdate).take(-1)
        val res = engine.evalAndExtractValue(q)
        assert(res.isEmpty)
      }

      "return the number of elements selected from the end of the list by fact ordering" in {
        val q = factsOfType(FactTypes.TagsUpdate).take(-1)
        val res = engine.evalAndExtractValue(q, FactTable(updateABC, updateDEF))
        assertResult(Seq(updateABC))(res)
      }

      "return all the elements of the list by fact ordering when the number requested is greater than the size" in {
        val q = factsOfType(FactTypes.TagsUpdate).take(-3)
        val res = engine.evalAndExtractValue(q, FactTable(updateABC, updateDEF))
        assertResult(Seq(updateDEF, updateABC))(res)
      }
    }

    "using .take(n) with 0" - {

      "return an empty collection" in {
        val q = factsOfType(FactTypes.TagsUpdate).take(0)
        val res = engine.evalAndExtractValue(q, FactTable(updateABC, updateDEF))
        assertResult(Seq.empty)(res)
      }
    }

    "using .headOption" - {

      "return None if the collection is empty" in {
        val q = factsOfType(FactTypes.TagsUpdate).headOption
        val res = engine.evalAndExtractValue(q)
        assertResult(None)(res)
      }

      "return the head of the collection by fact ordering" in {
        val q = factsOfType(FactTypes.TagsUpdate).headOption
        val res = engine.evalAndExtractValue(q, FactTable(updateABC, updateDEF))
        assertResult(Some(updateDEF))(res)
      }
    }
  }
}
