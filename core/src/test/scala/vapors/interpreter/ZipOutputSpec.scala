package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl._
import vapors.example.TagsUpdate

import org.scalatest.freespec.AnyFreeSpec
import shapeless.HNil

import java.time.Instant

class ZipOutputSpec extends AnyFreeSpec {

  "Expr.ZipOutput" - {

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

    "zippedToShortest should" - {

      "return a list as long as the shortest input list of" - {

        "tuple values" in {
          val query = wrapEach(
            const(List("A", "B", "C")),
            const(List(1, 2, 3, 4, 5)),
          ).zippedToShortest.asTuple
          val result = engine.evalAndExtractValue(query)
          assertResult(List(("A", 1), ("B", 2), ("C", 3))) {
            result
          }
        }

        "HList values" in {
          val query = wrapEach(
            const(List("A", "B", "C")),
            const(List(1, 2, 3, 4, 5)),
          ).zippedToShortest.asHList
          val result = engine.evalAndExtractValue(query)
          assertResult(List("A" :: 1 :: HNil, "B" :: 2 :: HNil, "C" :: 3 :: HNil)) {
            result
          }
        }

        "case class values" in {
          val query = wrapEach(
            const(List(tagsNow, tags5MinAgo, tags15MinAgo).map(_.source)),
            const(List(tagsNow, tags5MinAgo).map(_.tags)),
            const(List(tagsNow, tags5MinAgo, tags15MinAgo).map(_.timestamp)),
          ).zippedToShortest.as[TagsUpdate]
          val result = engine.evalAndExtractValue(query)
          assertResult(List(tagsNow, tags5MinAgo)) {
            result
          }
        }
      }

      "return nil when any input list is nil for" - {

        "tuple values" in {
          val query = wrapEach(
            const(List("A", "B", "C")),
            const(List.empty[Int]),
          ).zippedToShortest.asTuple
          val result = engine.evalAndExtractValue(query)
          assertResult(Nil) {
            result
          }
        }

        "HList values" in {
          val query = wrapEach(
            const(List.empty[String]),
            const(List(1, 2, 3, 4, 5)),
          ).zippedToShortest.asHList
          val result = engine.evalAndExtractValue(query)
          assertResult(Nil) {
            result
          }
        }

        "case class values" in {
          val query = wrapEach(
            const(List(tagsNow).map(_.source)),
            const(List(tagsNow, tags5MinAgo).map(_.tags)),
            const(List.empty[Instant]),
          ).zippedToShortest.asHList
          val result = engine.evalAndExtractValue(query)
          assertResult(Nil) {
            result
          }
        }
      }

      "not force a LazyList when" - {

        "taking the headOption" in {
          val query = wrapEach(
            const((tagsNow #:: fail("forced source") #:: LazyList.empty).map(_.source)),
            const((tagsNow #:: fail("forced tags") #:: LazyList.empty).map(_.tags)),
            const((tagsNow #:: fail("forced timestamp") #:: LazyList.empty).map(_.timestamp)),
          ).zippedToShortest.as[TagsUpdate].withOutputFoldable.headOption
          val result = engine.evalAndExtractValue(query)
          assertResult(Some(tagsNow)) {
            result
          }
        }
      }

      "not compile when" - {

        "given too many arguments for a case class" in {
          assertDoesNotCompile {
            """
              wrapEach(
                const(List.empty[String]),
                const(List.empty[Set[String]]),
                const(List.empty[Instant]),
                const(List.empty[Int]), // too many
              ).zippedToShortest.as[TagsUpdate]
            """
          }
        }

        "given too few arguments for a case class" in {
          assertDoesNotCompile {
            """
              wrapEach(
                const(List.empty[String]),
                const(List.empty[Set[String]]),
              ).zippedToShortest.as[TagsUpdate]
            """
          }
        }

        "given arguments out of order for a case class" in {
          assertDoesNotCompile {
            """
              wrapEach(
                const(List.empty[Set[String]]),
                const(List.empty[Instant]),
                const(List.empty[String]),
              ).zippedToShortest.as[TagsUpdate]
            """
          }
        }
      }
    }
  }
}
