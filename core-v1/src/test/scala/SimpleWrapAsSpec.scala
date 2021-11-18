package com.rallyhealth.vapors.v1

import example.GeoLocation

import munit.FunSuite
import shapeless._

class SimpleWrapAsSpec extends FunSuite {

  import dsl.simple._

  test("wrap lat / lng as a GeoLocation") {
    val expected = GeoLocation(1.0, 2.0)
    val q = (expected.lat :: expected.lng :: HNil).const.as[GeoLocation]
    val result = q.run()
    assertEquals(result, expected)
  }
}
