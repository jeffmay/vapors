package com.rallyhealth.vapors.core.interpreter

import com.rallyhealth.vapors.core.data.FactTable
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.example.TagsUpdate
import org.scalatest.freespec.AnyFreeSpec
import shapeless.HNil

import java.time.Instant

class ZipOutputSpec extends AnyFreeSpec {

  import com.rallyhealth.vapors.core.example.SimpleTagUpdates._

  "zip should" - {

    "return a list as long as the shortest input list of" - {

      "tuple values" in {
        val query = zip(
          const(List("A", "B", "C")),
          const(List(1, 2, 3, 4, 5)),
        ).asTuple
        val result = eval(FactTable.empty)(query)
        assertResult(List(("A", 1), ("B", 2), ("C", 3))) {
          result.output.value
        }
      }

      "HList values" in {
        val query = zip(
          const(List("A", "B", "C")),
          const(List(1, 2, 3, 4, 5)),
        ).asHList
        val result = eval(FactTable.empty)(query)
        assertResult(List("A" :: 1 :: HNil, "B" :: 2 :: HNil, "C" :: 3 :: HNil)) {
          result.output.value
        }
      }

      "case class values" in {
        val query = zip(
          const(List(tagsNow, tags5MinAgo, tags15MinAgo).map(_.source)),
          const(List(tagsNow, tags5MinAgo).map(_.tags)),
          const(List(tagsNow, tags5MinAgo, tags15MinAgo).map(_.timestamp)),
        ).as[TagsUpdate]
        val result = eval(FactTable.empty)(query)
        assertResult(List(tagsNow, tags5MinAgo)) {
          result.output.value
        }
      }
    }

    "return nil when any input list is nil for" - {

      "tuple values" in {
        val query = zip(
          const(List("A", "B", "C")),
          const(List.empty[Int]),
        ).asTuple
        val result = eval(FactTable.empty)(query)
        assertResult(Nil) {
          result.output.value
        }
      }

      "HList values" in {
        val query = zip(
          const(List.empty[String]),
          const(List(1, 2, 3, 4, 5)),
        ).asHList
        val result = eval(FactTable.empty)(query)
        assertResult(Nil) {
          result.output.value
        }
      }

      "case class values" in {
        val query = zip(
          const(List(tagsNow).map(_.source)),
          const(List(tagsNow, tags5MinAgo).map(_.tags)),
          const(List.empty[Instant]),
        ).asHList
        val result = eval(FactTable.empty)(query)
        assertResult(Nil) {
          result.output.value
        }
      }
    }

    "not compile when" - {

      "given too many arguments for a case class" in {
        assertDoesNotCompile {
          """
          zip(
            const(List.empty[String]),
            const(List.empty[Set[String]]),
            const(List.empty[Instant]),
            const(List.empty[Int]), // too many
          ).as[TagsUpdate]
        """
        }
      }

      "given too few arguments for a case class" in {
        assertDoesNotCompile {
          """
          zip(
            const(List.empty[String]),
            const(List.empty[Set[String]]),
          ).as[TagsUpdate]
        """
        }
      }

      "given arguments out of order for a case class" in {
        assertDoesNotCompile {
          """
          zip(
            const(List.empty[Set[String]]),
            const(List.empty[Instant]),
            const(List.empty[String]),
          ).as[TagsUpdate]
        """
        }
      }
    }
  }
}
