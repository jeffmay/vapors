package com.rallyhealth.vapors.v1

import data.Justified
import example.NestedSelectable
import lens.{DataPath, VariantLensMacros}

import cats.data.NonEmptySeq
import munit.FunSuite
import shapeless.Nat

class SimpleJustifiedSelectSpec extends FunSuite {

  import dsl.simple.justified._

  import NestedSelectable.empty

  test("Select a value of a const") {
    val expected = NestedSelectable("expectedValue")
    val expr = expected.const.get(_.select(_.value))
    val expectedWrapped = Justified.bySelection(expected.value, expr.lens.path, Justified.byConst(expected))
    val obtained = expr.run()
    assertEquals(obtained, expectedWrapped)
  }

  test("Select a nested Option value of a const") {
    val expected = NestedSelectable("expectedValue")
    val fixture = NestedSelectable("optional", Some(expected))
    val expr = fixture.const.get(_.select(_.opt)).map {
      _.get(_.select(_.value))
    }
    val expectedWrapped = Some(
      Justified.bySelection(
        expected.value,
        DataPath.empty.atField("opt").atIndex(0).atField("value"),
        Justified.byConst(fixture),
      ),
    )
    val obtained = expr.run()
    assertEquals(obtained, expectedWrapped)
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
    val firstValue = NestedSelectable("firstValue")
    val secondValue = NestedSelectable("secondValue")
    val fixture = NestedSelectable("seqDefined", seq = Seq(firstValue, secondValue))
    val expr = fixture.const.get(_.select(_.seq)).map(_.get(_.select(_.value)))
    val observed = expr.run()
    val expected = Seq(
      Justified.bySelection(
        firstValue.value,
        DataPath.empty.atField("seq").atIndex(0).atField("value"),
        Justified.byConst(fixture),
      ),
      Justified.bySelection(
        secondValue.value,
        DataPath.empty.atField("seq").atIndex(1).atField("value"),
        Justified.byConst(fixture),
      ),
    )
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

  test("Select the values of a nested Map value of a const") {
    val first = NestedSelectable("firstValue")
    val second = NestedSelectable("secondValue")
    val fixture = NestedSelectable("mapDefined", map = Map("one" -> first, "two" -> second))
    val expr = fixture.const.get(_.select(_.map).to[Seq]).map { kv =>
      kv.get(_.at(Nat._1).select(_.value))
    }
    val observed = expr.run()
    val expectedWrapped = Seq(
      Justified.bySelection(
        first.value,
        // TODO: How to keep the original map keys / remove the tuple keys here?
        DataPath.empty.atField("map").atIndex(0).atKey("1").atField("value"),
        Justified.byConst(fixture),
      ),
      Justified.bySelection(
        second.value,
        // TODO: How to keep the original map keys / remove the tuple keys here?
        DataPath.empty.atField("map").atIndex(1).atKey("1").atField("value"),
        Justified.byConst(fixture),
      ),
    )
    assertEquals(observed, expectedWrapped)
  }

  test("Cannot use the Lens.select operation to map over elements of a Map") {
    val fixture = NestedSelectable("seqDefined", seq = Seq(empty, empty))
    val _ =
      fixture.const.get(_.select(_.map).to[Seq]).map(_.get(_.at(Nat._1).select(_.value))) // correct
    val message = compileErrors {
      "fixture.const.get(_.select(_.map.map(_._2.value)))" // incorrect
    }
    assert(message contains VariantLensMacros.InvalidDataPathMessage)
  }

  test("Select a headOption from an Option") {
    val expected = NestedSelectable("firstElement")
    val fixture = NestedSelectable("optDefined", opt = Some(expected))
    val expr = fixture.const.get(_.select(_.opt)).headOption
    val observed = expr.run()
    val expectedWrapped = fixture.opt.map { v =>
      // TODO: Should this include the index for headOption on an Option?
      Justified.bySelection(v, DataPath.empty.atField("opt").atIndex(0), Justified.byConst(fixture))
    }
    assertEquals(expectedWrapped.map(_.value), Some(expected))
    assertEquals(observed, expectedWrapped)
  }

  test("Select None from an empty Option") {
    val fixture = NestedSelectable.empty
    val expr = fixture.const.get(_.select(_.opt)).headOption
    val observed = expr.run()
    assertEquals(observed, None)
  }

  test("Select a headOption from a Seq") {
    val expected = NestedSelectable("firstElement")
    val fixture = NestedSelectable("seqDefined", seq = Seq(expected, empty))
    val expr = fixture.const.get(_.select(_.seq)).headOption
    val observed = expr.run()
    val expectedWrapped = fixture.seq.headOption.map { v =>
      Justified.bySelection(v, DataPath.empty.atField("seq").atIndex(0), Justified.byConst(fixture))
    }
    assertEquals(expectedWrapped.map(_.value), Some(expected))
    assertEquals(observed, expectedWrapped)
  }

  test("Select None from an empty Seq") {
    val fixture = NestedSelectable.empty
    val expr = fixture.const.get(_.select(_.seq)).headOption
    val observed = expr.run()
    assertEquals(observed, None)
  }

  test("Cannot select a headOption from an unsorted Map") {
    val fixture = NestedSelectable("mapDefined", map = Map("one" -> empty, "two" -> empty))
    val message = compileErrors {
      "fixture.const.get(_.select(_.map)).headOption"
    }
    assert(message contains "value headOption is not a member")
  }

  test("Select a head from a NonEmptySeq") {
    val fixture = NonEmptySeq.of(1, 2, 3)
    val expr = fixture.const.head
    val observed = expr.run()
    val expected =
      Justified.bySelection(fixture.head, DataPath.empty.atIndex(0), Justified.byConst(fixture))
    assertEquals(observed, expected)
  }

  test("Cannot select a head from a Seq, even if non-empty") {
    val fixture = Seq(1, 2, 3)
    val message = compileErrors {
      "fixture.const.head"
    }
    assert(message contains "Could not find an instance of Reducible for Seq")
  }
}
