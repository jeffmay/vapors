//package com.rallyhealth.vapors.v1
//
//import example.FactTypes
//import dsl.uncached._
//
//import munit.FunSuite
//
//class SimpleUsingAritySpec extends FunSuite {
//
//  import SimpleUsingAritySpec._
//
//  test("using 1") {
//    val isMatch = using(defineAge25).thenReturn { ages =>
//      ages.exists(_ > 18.const)
//    }
//    assert(isMatch.run())
//  }
//
//  test("using 2") {
//    val isMatch = using(defineAge25, defineWeightLbs150).thenReturn { (ages, weights) =>
//      ages.exists(_ > 18.const) && weights.forall(_ < 200d.const)
//    }
//    assert(isMatch.run())
//  }
//
//  test("using 3 repeat 1") {
//    val isMatch = using(defineAge25, defineWeightLbs150, defineAge16).thenReturn { (ages, weights, _) =>
//      ages.exists(_ > 18.const) && weights.forall(_ < 200d.const)
//    }
//    assert(isMatch.run())
//  }
//
//  test("using 4 repeat 2") {
//    val isMatch = using(defineAge25, defineWeightLbs150, defineAge16, defineWeightLbs250).thenReturn {
//      (ages, weights, _, _) =>
//        ages.exists(_ > 18.const) && weights.forall(_ < 200d.const)
//    }
//    assert(!isMatch.run())
//  }
//}
//
//object SimpleUsingAritySpec {
//
//  final val defineAge16 = define(FactTypes.Age).oneFromConst(16)
//  final val defineAge25 = define(FactTypes.Age).oneFromConst(25)
//  final val defineWeightLbs150 = define(FactTypes.WeightLbs).oneFromConst(150)
//  final val defineWeightLbs250 = define(FactTypes.WeightLbs).oneFromConst(250)
//}
