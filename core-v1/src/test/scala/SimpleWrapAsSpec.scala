//package com.rallyhealth.vapors.v1
//
//import example.GeoLocation
//
//import munit.FunSuite
//
//class SimpleWrapAsSpec extends FunSuite {
//
//  import dsl.uncached._
//
//  test("wrap lat / lng as a GeoLocation") {
//    val expected = GeoLocation(1.0, 2.0)
//    val q = (expected.lat, expected.lngs).const.as[GeoLocation]
//    val result = q.run()
//    assertEquals(result, expected)
//  }
//}
