package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.core.data.{Evidence, FactTable}
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.factfilter.Example.{ColorCoding, FactTypes, JoeSchmoe, TagsUpdate}
import org.scalatest.wordspec.AnyWordSpec
import shapeless.HNil

import java.time.Instant

class WrapOutputSpec extends AnyWordSpec {

  "Expr.WrapOutput" when {

    "wrapping const nodes into a case class" should {

      "wrap some constant nodes as a case class" in {
        val expected = TagsUpdate(Set("X"), Instant.now())
        val query = {
          wrap(const(expected.tags), const(expected.timestamp)).as[TagsUpdate]
        }
        val result = eval(FactTable.empty)(query)
        assertResult(expected)(result.output.value)
      }

      "NOT compile when attempting to wrap one less constant than required into a case class" in {
        val t = TagsUpdate(Set("X"), Instant.now())
        assertDoesNotCompile {
          "wrap(const(t.tags)).as[TagsUpdate]"
        }
      }

      "NOT compile when attempting to wrap some constant nodes into a case class in the wrong order" in {
        val t = TagsUpdate(Set("X"), Instant.now())
        assertDoesNotCompile {
          "wrap(const(t.timestamp), const(t.tags)).as[TagsUpdate]"
        }
      }

    }

    "wrapping const nodes into a tuple" should {

      "convert into a tuple-3" in {
        val t = (1, "two", ColorCoding.Blue)
        val query = {
          wrap(const(t._1), const(t._2), const(t._3)).asTuple
        }
        val result = eval(FactTable.empty)(query)
        assertResult(t)(result.output.value)
      }
    }

    "wrapping const nodes into an hlist" should {

      "return an hlist of size 3" in {
        val hlist = 1 :: "two" :: ColorCoding.Blue :: HNil
        val t = hlist.tupled
        val query = {
          wrap(const(t._1), const(t._2), const(t._3)).asHList
        }
        val result = eval(FactTable.empty)(query)
        assertResult(hlist)(result.output.value)
      }
    }

    "comparing evidence" should {

      "combine all non-empty evidence" in {
        val query = {
          wrap(
            factsOfType(FactTypes.Name).headOption,
            factsOfType(FactTypes.Age).headOption,
          ).asHList
        }
        val facts = List(JoeSchmoe.name, JoeSchmoe.age)
        val result = eval(FactTable(facts))(query)
        assertResult(Evidence(facts))(result.output.evidence)
      }

      "return no evidence if any branch has no evidence" in {
        val query = {
          wrap(
            factsOfType(FactTypes.Name).headOption,
            factsOfType(FactTypes.Age).headOption,
          ).asHList
        }
        val result = eval(FactTable(JoeSchmoe.name))(query)
        assertResult(Evidence.none)(result.output.evidence)
      }
    }
  }

}
