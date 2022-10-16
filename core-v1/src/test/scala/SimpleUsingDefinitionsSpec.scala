//package com.rallyhealth.vapors.v1
//
//import data.FactTable
//import example.FactTypes
//
//import munit.FunSuite
//
//class SimpleUsingDefinitionsSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  test("Derive Kg from Lbs") {
//    val weightLbs = FactTypes.WeightLbs(150)
//    val lbsPerKg = 2.205
//    val defineWeightKg = define(FactTypes.WeightKg).from {
//      valuesOfType(FactTypes.WeightLbs).map { lbs =>
//        lbs / lbsPerKg.const
//      }
//    }
//    val expr = using(defineWeightKg).thenReturn {
//      _.map(_ * 2d.const)
//    }
//    val observed = expr.run(FactTable(weightLbs))
//    val expected = (weightLbs.value / lbsPerKg) * 2
//    assert(observed.sizeIs == 1)
//    assertEqualsDouble(observed.head, expected, 0.0000001)
//  }
//
//}
