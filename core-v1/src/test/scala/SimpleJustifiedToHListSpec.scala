package com.rallyhealth.vapors.v1

import data.Justified
import example.Person
import lens.DataPath

import cats.data.NonEmptySeq
import munit.FunSuite
import shapeless.HNil

class SimpleJustifiedToHListSpec extends FunSuite {

  import dsl.uncached.justified._

  private val alice = Person("Alice", 40)
  private val bob = Person("Bob", 45)

  test("(String :: Int).toHList.as[Person]") {
    val expected = alice
    val xhl = expected.name.const :: expected.age.const
    val expr: Any ~:> Justified[Person] = xhl.toHList.as[Person]
    val res = expr.run()
    val expectedWrapped = Justified.bySelection(
      expected,
      DataPath.empty,
      Justified.byInference(
        "map",
        expected.name :: expected.age :: HNil,
        NonEmptySeq.of(
          Justified.byInference(
            "product",
            (expected.name, expected.age :: HNil),
            NonEmptySeq.of(
              Justified.byConst(expected.name),
              Justified.byInference(
                "map",
                expected.age :: HNil,
                NonEmptySeq.of(Justified.byConst(expected.age)),
              ),
            ),
          ),
        ),
      ),
    )
    assertEquals(res, expectedWrapped)
  }

  test("wrap(String, Int).as[Person]") {
    val expected = alice
    val xhl = wrap(expected.name.const, expected.age.const)
    val expr: Any ~:> Justified[Person] = xhl.toHList.as[Person]
    val res = expr.run()
    val expectedWrapped = Justified.bySelection(
      expected,
      DataPath.empty,
      Justified.byInference(
        "map",
        expected.name :: expected.age :: HNil,
        NonEmptySeq.of(
          Justified.byInference(
            "product",
            (expected.name, expected.age :: HNil),
            NonEmptySeq.of(
              Justified.byConst(expected.name),
              Justified.byInference(
                "map",
                expected.age :: HNil,
                NonEmptySeq.of(Justified.byConst(expected.age)),
              ),
            ),
          ),
        ),
      ),
    )
    assertEquals(res, expectedWrapped)
  }

  test("wrap(Seq[String], String[Int]).zipToShortest") {
    val expected = Seq(alice, bob)
    val names = expected.map(_.name)
    val ages = expected.map(_.age)
    val xhl = wrap(names.const, ages.const).zipToShortest
    val expr = xhl.map { hl =>
      hl.as[Person]
    }
    val observed = expr.run()
    val expectedWrapped = Vector(
      Justified.bySelection(
        alice,
        DataPath.empty,
        Justified.byInference(
          "map",
          alice.name :: alice.age :: HNil,
          NonEmptySeq.of(
            Justified.byInference(
              "product",
              (alice.name, alice.age :: HNil),
              NonEmptySeq.of(
                Justified.bySelection(alice.name, DataPath.empty.atIndex(0), Justified.byConst(names)),
                Justified.byInference(
                  "map",
                  alice.age :: HNil,
                  NonEmptySeq.of(
                    Justified.bySelection(alice.age, DataPath.empty.atIndex(0), Justified.byConst(ages)),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
      Justified.bySelection(
        bob,
        DataPath.empty,
        Justified.byInference(
          "map",
          bob.name :: bob.age :: HNil,
          NonEmptySeq.of(
            Justified.byInference(
              "product",
              (bob.name, bob.age :: HNil),
              NonEmptySeq.of(
                Justified.bySelection(bob.name, DataPath.empty.atIndex(1), Justified.byConst(names)),
                Justified.byInference(
                  "map",
                  bob.age :: HNil,
                  NonEmptySeq.of(
                    Justified.bySelection(bob.age, DataPath.empty.atIndex(1), Justified.byConst(ages)),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    )
    assertEquals(observed, expectedWrapped)
  }
}
