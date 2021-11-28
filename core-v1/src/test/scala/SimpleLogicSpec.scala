package com.rallyhealth.vapors.v1

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class SimpleLogicSpec extends ScalaCheckSuite {

  import dsl.simple._

  property("Boolean && Boolean") {
    forAll { (l: Boolean, r: Boolean) =>
      val expected = l && r
      val obtained = (l.const && r.const).run()
      assertEquals(obtained, expected)
    }
  }

  property("and(Boolean, Boolean)") {
    forAll { (l: Boolean, r: Boolean) =>
      val expected = l && r
      val obtained = and(l.const, r.const).run()
      assertEquals(obtained, expected)
    }
  }

  property("Boolean || Boolean") {
    forAll { (l: Boolean, r: Boolean) =>
      val expected = l || r
      val obtained = (l.const || r.const).run()
      assertEquals(obtained, expected)
    }
  }

  property("or(Boolean, Boolean)") {
    forAll { (l: Boolean, r: Boolean) =>
      val expected = l || r
      val obtained = or(l.const, r.const).run()
      assertEquals(obtained, expected)
    }
  }
}
