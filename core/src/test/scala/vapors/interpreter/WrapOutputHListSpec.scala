package com.rallyhealth

package vapors.interpreter

import vapors.data.{Evidence, FactTable}
import vapors.dsl._
import vapors.example.{ColorCoding, FactTypes, JoeSchmoe, TagsUpdate}

import org.scalatest.freespec.AnyFreeSpec
import shapeless.HNil

class WrapOutputHListSpec extends AnyFreeSpec {

  "Expr.WrapOutputHList" - {

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
    import vapors.example.SimpleTagUpdates._

    "wrap, when given const nodes, should" - {

      "convert to a case class with the correct number of inputs" in {
        val query = {
          wrap(const(tagsNow.source), const(tagsNow.tags), const(tagsNow.timestamp)).as[TagsUpdate]
        }
        val result = engine.evalAndExtractValue(query)
        assertResult(tagsNow)(result)
      }

      "NOT compile when attempting to convert fewer inputs than required into a case class" in {
        assertDoesNotCompile {
          "wrap(const(tagsNow.tags)).as[TagsUpdate]"
        }
      }

      "NOT compile when attempting to convert inputs into a case class in the wrong order" in {
        assertDoesNotCompile {
          "wrap(const(tagsNow.tags), const(tagsNow.timestamp), const(tagsNow.source)).as[TagsUpdate]"
        }
      }

      "convert into a tuple-3" in {
        val t = (1, "two", ColorCoding.Blue)
        val query = {
          wrap(const(t._1), const(t._2), const(t._3)).asTuple
        }
        val result = engine.evalAndExtractValue(query)
        assertResult(t)(result)
      }

      "convert into an hlist of size 3" in {
        val hlist = 1 :: "two" :: ColorCoding.Blue :: HNil
        val t = hlist.tupled
        val query = {
          wrap(const(t._1), const(t._2), const(t._3)).asHList
        }
        val result = engine.evalAndExtractValue(query)
        assertResult(hlist)(result)
      }
    }

    "wrap, when comparing evidence, should" - {

      "combine all non-empty evidence" in {
        val query = {
          wrap(
            factsOfType(FactTypes.Name).headOption,
            factsOfType(FactTypes.Age).headOption,
          ).asHList
        }
        val facts = List(JoeSchmoe.name, JoeSchmoe.age)
        val result = engine.eval(query, FactTable(facts))
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence(facts)) {
            engine.extract(evidence)
          }
        }
      }

      "return no evidence if any branch has no evidence" in {
        val query = {
          wrap(
            factsOfType(FactTypes.Name).headOption,
            factsOfType(FactTypes.Age).headOption,
          ).asHList
        }
        val result = engine.eval(query, FactTable(JoeSchmoe.name))
        for (evidence <- result.maybeEvidence) {
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      }
    }
  }

}
