package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.Example.{ColorCoding, FactTypes, JoeSchmoe, TagsUpdate}
import com.rallyhealth.vapors.factfilter.data.{Evidence, FactTable}
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import org.scalatest.wordspec.AnyWordSpec
import shapeless.HNil

import java.time.Instant

class WrapOutputSpec extends AnyWordSpec {

  "Expr.WrapOutput" when {

    "wrapping const nodes into a case class" should {

      "wrap some constant nodes as a case class" in {
        val f = TagsUpdate(Set("X"), Instant.now())
        val q = {
          wrap(const(f.tags), const(f.timestamp)).as[TagsUpdate]
        }
        val result = eval(FactTable.empty)(q)
        assertResult(f)(result.output.value)
      }

      "NOT compile when attempting to wrap one less constant than required into a case class" in {
        val f = TagsUpdate(Set("X"), Instant.now())
        assertDoesNotCompile {
          "wrap(const(f.tags)).as[TagsUpdate]"
        }
      }

      "NOT compile when attempting to wrap some constant nodes into a case class in the wrong order" in {
        val f = TagsUpdate(Set("X"), Instant.now())
        assertDoesNotCompile {
          "wrap(const(f.timestamp), const(f.tags)).as[TagsUpdate]"
        }
      }

    }

    "wrapping const nodes into a tuple" should {

      "convert into a tuple-3" in {
        val t = (1, "two", ColorCoding.Blue)
        val q = {
          wrap(const(t._1), const(t._2), const(t._3))
            .as[(Int, String, ColorCoding.Blue.type)]
        }
        val result = eval(FactTable.empty)(q)
        assertResult(t)(result.output.value)
      }
    }

    "wrapping const nodes into an hlist" should {

      "return an hlist of size 3" in {
        val hlist = 1 :: "two" :: ColorCoding.Blue :: HNil
        val t = hlist.tupled
        val q = {
          wrap(const(t._1), const(t._2), const(t._3)).asHList
        }
        val result = eval(FactTable.empty)(q)
        assertResult(hlist)(result.output.value)
      }
    }

    "comparing evidence" should {

      "combine all non-empty evidence" in {
        val q = {
          wrap(
            factsOfType(FactTypes.Name).headOption,
            factsOfType(FactTypes.Age).headOption,
          ).asHList
        }
        val facts = List(JoeSchmoe.name, JoeSchmoe.age)
        val result = eval(FactTable(facts))(q)
        assertResult(Evidence(facts))(result.output.evidence)
      }

      "return no evidence if any branch has no evidence" in {
        val q = {
          wrap(
            factsOfType(FactTypes.Name).headOption,
            factsOfType(FactTypes.Age).headOption,
          ).asHList
        }
        val result = eval(FactTable(JoeSchmoe.name))(q)
        assertResult(Evidence.none)(result.output.evidence)
      }
    }
  }

}
