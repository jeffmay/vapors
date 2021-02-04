package com.rallyhealth.vapors.core.interpreter

import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.{DerivedFactOfType, Evidence, FactTable, FactType}
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.example.{FactTypes, JoeSchmoe, Role}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class UsingDefinitionsExprSpec extends AnyWordSpec {

  "Expr.UsingDefinitions" should {

    // TODO: Do the real computation here
    // TODO: Make a named function to call out to "dateCompare()" (maybe a primitive algebra?)
    //       Or just hack it for now.
    // now.year - dob.year - (if (now.dayOfYear < (dob.dayOfYear + (if (dob.isLeapYear) 1 else 0)) 1 else 0)
    lazy val ageFromDateOfBirth = {
      val now = LocalDate.now()
      withFactsOfType(FactTypes.DateOfBirth).where { facts =>
        facts.map { fact =>
          fact.get(_.select(_.value).select(_.getYear)).subtractFrom(now.getYear)
        }
      }
    }

    lazy val ageFromDateOfBirthDef: Expr.Definition[Unit] = {
      define(FactTypes.Age).fromEvery {
        ageFromDateOfBirth
      }
    }

    lazy val isOver18: RootExpr[Boolean, Unit] = {
      usingDefinitions(ageFromDateOfBirthDef) {
        withFactsOfType(FactTypes.Age).where {
          _.exists {
            _.get(_.select(_.value)) >= 18
          }
        }
      }
    }

    lazy val isUser: RootExpr[Boolean, Unit] = {
      withFactsOfType(FactTypes.Role).where {
        _.exists {
          _.get(_.select(_.value)) >= Role.User
        }
      }
    }

    lazy val isEligible: RootExpr[Boolean, Unit] = and(isOver18, isUser)

    lazy val isEligibleDef = define(FactType[Boolean]("is_eligible")).from(isEligible)

    "add the defined facts to the fact table" in {
      val result = eval(JoeSchmoe.factTable) {
        usingDefinitions(ageFromDateOfBirthDef) {
          withFactsOfType(FactTypes.Age).returnInput
        }
      }
      val ages = result.output.value.map(_.value)
      ages should contain only JoeSchmoe.age.value
    }

    "return the result of an expression with the Definition-produced facts as DerivedFacts" in {
      val yearWhen19 = LocalDate.now().getYear - 19
      val expiredAge = FactTypes.Age(17)
      val dob = FactTypes.DateOfBirth(LocalDate.of(yearWhen19, 1, 1))
      val facts = FactTable(expiredAge, dob)
      val result = eval(facts)(isOver18)
      assert(result.output.value)
      val ageFromYear = LocalDate.now().getYear - dob.value.getYear
      val expectedDerivedFact = DerivedFactOfType(FactTypes.Age, ageFromYear, Evidence(dob))
      assertResult(Evidence(expectedDerivedFact))(result.output.evidence)
      assertResult(Evidence(dob))(result.output.evidence.derivedFromSources)
    }

    "support nested fact definitions" in {
      val yearWhen19 = LocalDate.now().getYear - 19
      val userRole = FactTypes.Role(Role.User)
      val expiredAge = FactTypes.Age(17)
      val dob = FactTypes.DateOfBirth(LocalDate.of(yearWhen19, 1, 1))
      val facts = FactTable(expiredAge, dob, userRole)
      val result = eval(facts) {
        usingDefinitions(isEligibleDef) {
          withFactsOfType(isEligibleDef.factType).where {
            _.exists(_.get(_.select(_.value)))
          }
        }
      }
      assert(result.output.value)
      val correctAge = DerivedFactOfType(FactTypes.Age, 19, Evidence(dob))
      val evidenceOfEligibility = Evidence(
        DerivedFactOfType(isEligibleDef.factType, true, Evidence(correctAge, userRole)),
      )
      assertResult(evidenceOfEligibility)(result.output.evidence)
      assertResult(Evidence(dob, userRole))(result.output.evidence.derivedFromSources)
    }

  }
}
