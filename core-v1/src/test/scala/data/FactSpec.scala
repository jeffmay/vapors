package com.rallyhealth.vapors.v1

package data

import munit.FunSuite
import example.FactTypes

import java.time.LocalDate

class FactSpec extends FunSuite {

  test("Fact extractor matches CustomFact") {
    val f = CustomFact(FactTypes.Age, 23)
    f match {
      case Fact(tpe, value) =>
        assert(tpe == f.typeInfo && value == f.value)
    }
  }

  test("DerivedFact extractor matches CustomDerivedFact") {
    val yearsAgo23 = FactTypes.DateOfBirth(LocalDate.now().minusYears(23).withDayOfYear(1))
    val f = CustomDerivedFact(FactTypes.Age, 23, Evidence(yearsAgo23))
    f match {
      case DerivedFact(tpe, value, evidence) =>
        assert(tpe == f.typeInfo && value == f.value && evidence == f.evidence)
    }
  }
}

final case class CustomFact[T](
  typeInfo: FactType[T],
  value: T,
) extends Fact {
  override type Value = T
}

final case class CustomDerivedFact[T](
  typeInfo: FactType[T],
  value: T,
  evidence: Evidence,
) extends DerivedFact {
  override type Value = T
}
