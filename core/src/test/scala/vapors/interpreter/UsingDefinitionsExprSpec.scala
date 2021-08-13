package com.rallyhealth

package vapors.interpreter

import vapors.data.{DerivedFactOfType, Evidence, FactTable}
import vapors.dsl._
import vapors.example.{FactTypes, JoeSchmoe, Role, Snippets}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import java.time.LocalDate

class UsingDefinitionsExprSpec extends AnyFreeSpec {

  "Expr.UsingDefinitions" - {

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

    "add the defined facts to the fact table" in {
      val ages = engine.evalAndExtractValue(
        usingDefinitions(Snippets.ageFromDateOfBirthDef) {
          valuesOfType(FactTypes.Age)
        },
        JoeSchmoe.factTable,
      )
      ages should contain only JoeSchmoe.age.value
    }

    "return the result of an expression with the Definition-produced facts as DerivedFacts" in {
      val yearWhen19 = LocalDate.now().getYear - 19
      val expiredAge = FactTypes.Age(17)
      val dob = FactTypes.DateOfBirth(LocalDate.of(yearWhen19, 1, 1))
      val facts = FactTable(expiredAge, dob)
      val result = engine.eval(Snippets.isOver18, facts)
      val resultValue = engine.extract(result.value)
      assert(resultValue)
      for (evidence <- result.maybeEvidence) {
        val ageFromYear = LocalDate.now().getYear - dob.value.getYear
        val expectedDerivedFact = DerivedFactOfType(FactTypes.Age, ageFromYear, Evidence(dob))
        val resEvidence = engine.extract(evidence)
        assertResult(Evidence(expectedDerivedFact))(resEvidence)
        assertResult(Evidence(dob))(resEvidence.derivedFromSources)
      }
    }

    "support nested fact definitions" in {
      val yearWhen19 = LocalDate.now().getYear - 19
      val userRole = FactTypes.Role(Role.User)
      val expiredAge = FactTypes.Age(17)
      val dob = FactTypes.DateOfBirth(LocalDate.of(yearWhen19, 1, 1))
      val facts = FactTable(expiredAge, dob, userRole)
      val definition = Snippets.isEligibleDef
      val result = engine.eval(
        usingDefinitions(definition) {
          factsOfType(definition.factType).exists {
            _.get(_.select(_.value))
          }
        },
        facts,
      )
      val resultValue = engine.extract(result.value)
      assert(resultValue)
      for (evidence <- result.maybeEvidence) {
        val correctAge = DerivedFactOfType(FactTypes.Age, 19, Evidence(dob))
        val evidenceOfEligibility = Evidence(
          DerivedFactOfType(definition.factType, true, Evidence(correctAge, userRole)),
        )
        val resultEvidence = engine.extract(evidence)
        assertResult(evidenceOfEligibility)(resultEvidence)
        assertResult(Evidence(dob, userRole))(resultEvidence.derivedFromSources)
      }
    }

  }
}
