package com.rallyhealth.vapors.core.puml

import com.rallyhealth.vapors.core.Example.FactTypes
import com.rallyhealth.vapors.core.dsl._
import org.scalatest.wordspec.AnyWordSpec

class PlantUmlSpec extends AnyWordSpec {

  "PlantUml" should {

    "convert an expression into a graph and prints it" in {
      val q = {
        __.withFactsOfType(FactTypes.Age)
          .whereAnyValue {
            __ > 30
          }
      }
      val graph = PlantUml.createGraph(q)
      assertResult(
        """
          |"EXISTS"
          |"WITHIN: x > 30"
          |
          |"WITHIN: x > 30" --> "EXISTS"
          |""".stripMargin.trim,
      ) {
        PlantUml.serialize(graph)
      }
    }
  }
}
