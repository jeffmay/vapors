package com.rallyhealth.vapors.v1

import com.rallyhealth.vapors.v1.lens.VariantLensMacros
import example.NestedSelectable
import example.NestedSelectable.empty
import munit.FunSuite

class SimpleSelectSpec extends FunSuite {

  import dsl.simple._

  test("Select a value of a const") {
    val fixture = NestedSelectable.empty
    val expr = fixture.const.get(_.select(_.value))
    val observed = expr.run()
    val expected = fixture.value
    assertEquals(observed, expected)
  }

  test("Select a nested Option value of a const") {
    val fixture = NestedSelectable("optDefined", opt = Some(empty))
    val expr = fixture.const.get(_.select(_.opt)).map(_.get(_.select(_.value)))
    val observed = expr.run()
    val expected = fixture.opt.map(_.value)
    assertEquals(observed, expected)
  }

  test("Cannot use the Lens.select operation to map over elements of an Option") {
    val fixture = NestedSelectable("optDefined", opt = Some(empty))
    val _ =
      fixture.const.get(_.select(_.opt)).map(_.get(_.select(_.value))) // correct
    val message = compileErrors {
      "fixture.const.get(_.select(_.opt.map(_.value)))" // incorrect
    }
    assert(message contains VariantLensMacros.InvalidDataPathMessage)
  }

  test("Select a nested Seq value of a const") {
    val fixture = NestedSelectable("seqDefined", seq = Seq(empty, empty))
    val expr = fixture.const.get(_.select(_.seq)).map(_.get(_.select(_.value)))
    val observed = expr.run()
    val expected = fixture.seq.map(_.value)
    assertEquals(observed, expected)
  }

  test("Cannot use the Lens.select operation to map over elements of a Seq") {
    val fixture = NestedSelectable("seqDefined", seq = Seq(empty, empty))
    val _ =
      fixture.const.get(_.select(_.seq)).map(_.get(_.select(_.value))) // correct
    val message = compileErrors {
      "fixture.const.get(_.select(_.seq.map(_.value)))" // incorrect
    }
    assert(message contains VariantLensMacros.InvalidDataPathMessage)
  }

  test("Select a nested Map value of a const") {
    val fixture = NestedSelectable("mapDefined", map = Map("one" -> empty, "two" -> empty))
    val expr = fixture.const.get(_.select(_.map)).map(_.get(_.select(_.value)))
    val observed = expr.run()
    // TODO: Support a .mapPairs / .mapItems / .mapKV method
    // TODO: Support other map operations
    val expected = fixture.map.map { case (k, v) => (k, v.value) }
    assertEquals(observed, expected)
  }

  test("Cannot use the Lens.select operation to map over elements of a Map") {
    val fixture = NestedSelectable("seqDefined", seq = Seq(empty, empty))
    val _ =
      fixture.const.get(_.select(_.map)).map(_.get(_.select(_.value))) // correct
    val message = compileErrors {
      "fixture.const.get(_.select(_.map.map { case (k, v) => (k, v.value) }))" // incorrect
    }
    assert(message contains VariantLensMacros.InvalidDataPathMessage)
  }
}
