package com.rallyhealth.vapors.v1

import algebra.ZipToHList
import data.FactTable
import engine.SimpleEngine

import munit.FunSuite
import shapeless.HNil

class SimpleZipToHListSpec extends FunSuite {

  import dsl.simple._

  test("hlist const") {
    val x = 1.const :: "a".const
    val compute = x.zipToHListWith(ZipToHList.proxy(SimpleEngine[OP](FactTable.empty)))
    val res = compute(())
    assertEquals(res, 1 :: "a" :: HNil)
  }

}
