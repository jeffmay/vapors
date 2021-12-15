package com.rallyhealth.vapors.v1

import data.Justified
import example.NestedSelectable
import cats.data.NonEmptyList
import lens.VariantLensMacros
import munit.FunSuite

class SimpleJustifiedSelectSpec extends FunSuite {

  import dsl.simple.justified._

  import NestedSelectable.empty

  test("Select a value of a const") {
    val root = empty.const
    val expr = root.get(_.select(_.value))
    val expected = Justified.byInference(
      "select(_.value)",
      root.value.value.value,
      NonEmptyList.of(root.value),
    )
    val obtained = expr.run()
    assertEquals(obtained, expected)
  }

  test("Select a nested Option value of a const") {
    val fixture = NestedSelectable("optional", Some(empty))
    val root = NestedSelectable("optional", Some(empty)).const
    val expr = root.get(_.select(_.opt)).map {
      _.get(_.select(_.value))
    }
    val expected = Some(
      Justified.byInference(
        "select(_.value)",
        "empty",
        NonEmptyList.of(
          Justified.byInference(
            "select(_.opt)",
            empty,
            NonEmptyList.of(Justified.byConst(fixture)),
          ),
        ),
      ),
    )
    val obtained = expr.run()
    assertEquals(obtained, expected)
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
    val expected = Seq(
      Justified.byInference(
        "select(_.value)",
        "empty",
        NonEmptyList.of(
          Justified.byInference(
            "select(_.seq)",
            empty,
            NonEmptyList.of(Justified.byConst(fixture)),
          ),
        ),
      ),
      Justified.byInference(
        "select(_.value)",
        "empty",
        NonEmptyList.of(
          Justified.byInference(
            "select(_.seq)",
            empty,
            NonEmptyList.of(Justified.byConst(fixture)),
          ),
        ),
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

  test("Select a nested Map value of a const") {
    val fixture = NestedSelectable("mapDefined", map = Map("one" -> empty, "two" -> empty))
    val expr = fixture.const.get(_.select(_.map)).map(_.get(_.select(_.value)))
    val observed = expr.run()
    val expected = Map(
      "one" -> Justified.byInference(
        "select(_.value)",
        "empty",
        NonEmptyList.of(
          Justified.byInference(
            "select(_.map)",
            empty,
            NonEmptyList.of(Justified.byConst(fixture)),
          ),
        ),
      ),
      "two" -> Justified.byInference(
        "select(_.value)",
        "empty",
        NonEmptyList.of(
          Justified.byInference(
            "select(_.map)",
            empty,
            NonEmptyList.of(Justified.byConst(fixture)),
          ),
        ),
      ),
    )
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

  test("Select a headOption from an Option") {
    val expectedValueString = "firstElement"
    val fixture = NestedSelectable("optDefined", opt = Some(NestedSelectable(expectedValueString)))
    val expr = fixture.const.get(_.select(_.opt)).headOption
    val observed = expr.run()
    val expected = fixture.opt.map { v =>
      Justified.byInference("select(_.opt)", v, NonEmptyList.of(Justified.byConst(fixture)))
    }
    assertEquals(expected.map(_.value.value), Some(expectedValueString))
    assertEquals(observed, expected)
  }

  test("Select None from an empty Option") {
    val fixture = NestedSelectable.empty
    val expr = fixture.const.get(_.select(_.opt)).headOption
    val observed = expr.run()
    assertEquals(observed, None)
  }

  test("Select a headOption from a Seq") {
    val expectedValueString = "firstElement"
    val fixture = NestedSelectable("seqDefined", seq = Seq(NestedSelectable(expectedValueString), empty))
    val expr = fixture.const.get(_.select(_.seq)).headOption
    val observed = expr.run()
    val expected = fixture.seq.headOption.map { v =>
      // TODO: The Path should include the .headOption selection
      // Justified.byInference("select(_.seq[0])", v, NonEmptyList.of(Justified.byConst(fixture)))
      Justified.byInference("select(_.seq)", v, NonEmptyList.of(Justified.byConst(fixture)))
    }
    assertEquals(expected.map(_.value.value), Some(expectedValueString))
    assertEquals(observed, expected)
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
    assert(message contains "Could not find an instance of Foldable for [+V]scala.collection.immutable.Map[String,V]")
  }
}
