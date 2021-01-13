package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.Example.{FactTypes, Tags}
import com.rallyhealth.vapors.factfilter.data.{Evidence, FactTable}
import org.scalatest.wordspec.AnyWordSpec
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._

class FilterOutputSpec extends AnyWordSpec {

  private val sampleFactTable = FactTable(Seq(Tags.smoker, Tags.asthma, Tags.normalBmi))

  "Expr.FilterOutput" when {

    "using containsAny op" should {

      "return 'true' when the fact table contains a superset of the given set" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).containsAny(Set(Tags.asthma).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        assert(res.output.value)
      }

      "return the correct evidence for the facts that contain a superset of the given set" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).containsAny(Set(Tags.asthma).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        pendingUntilFixed {
          assertResult(Evidence(Tags.asthma))(res.output.evidence)
        }
      }

      "return 'true' when the fact table contains a subset of the given set" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).containsAny(Set(Tags.asthma, Tags.obeseBmi).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        assert(res.output.value)
      }

      "return the correct evidence for the facts that contain a subset of the given set" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).containsAny(Set(Tags.asthma, Tags.obeseBmi).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        pendingUntilFixed {
          assertResult(Evidence(Tags.asthma))(res.output.evidence)
        }
      }

      "return 'false' when the facts do not contain anything in the given set" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).containsAny(Set(Tags.obeseBmi).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        assert(!res.output.value)
      }

      "return empty evidence when the fact table does not contain anything in the given set" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).containsAny(Set(Tags.obeseBmi).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        pendingUntilFixed {
          assert(res.output.evidence.isEmpty)
        }
      }
    }

    "using filter op" should {

      "return all matching facts from a given subset" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.filter(Set(Tags.asthma))
        }
        val res = eval(sampleFactTable)(q)
        assertResult(Set(Tags.asthma))(res.output.value)
      }

      "return all matching values from a given subset" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).filter(Set(Tags.asthma).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        assertResult(List(Tags.asthma).map(_.value))(res.output.value)
      }

      "return the correct evidence for the matching values from a given subset" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).filter(Set(Tags.asthma).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        pendingUntilFixed {
          assertResult(Evidence(Tags.asthma))(res.output.evidence)
        }
      }

      "return all matching facts from a given superset" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.filter(Set(Tags.asthma, Tags.obeseBmi))
        }
        val res = eval(sampleFactTable)(q)
        assertResult(Set(Tags.asthma))(res.output.value)
      }

      "return the matching values from a given superset" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).filter(Set(Tags.asthma, Tags.obeseBmi).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        assertResult(List(Tags.asthma).map(_.value))(res.output.value)
      }

      "return the correct evidence for the matching values from a given superset" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).filter(Set(Tags.asthma, Tags.obeseBmi).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        pendingUntilFixed {
          assertResult(Evidence(Tags.asthma))(res.output.evidence)
        }
      }

      "return an empty list of facts when given a set that contains no common elements" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.filter(Set(Tags.obeseBmi))
        }
        val res = eval(sampleFactTable)(q)
        assert(res.output.value.isEmpty)
      }

      "return an empty list of values when given a set that contains no common elements" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).filter(Set(Tags.obeseBmi).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        assert(res.output.value.isEmpty)
      }

      "return empty evidence when given a set that contains no common elements" in {
        val q = withFactsOfType(FactTypes.Tag).where { facts =>
          facts.toList.map(_.value).filter(Set(Tags.obeseBmi).map(_.value))
        }
        val res = eval(sampleFactTable)(q)
        pendingUntilFixed {
          assert(res.output.evidence.isEmpty)
        }
      }
    }
  }
}
