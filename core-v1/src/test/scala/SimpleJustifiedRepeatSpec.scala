package com.rallyhealth.vapors.v1

import data.{FactTable, Justified}
import example.FactTypes
import lens.DataPath

import cats.data.NonEmptySeq
import munit.FunSuite
import shapeless.{HNil, Nat}

class SimpleJustifiedRepeatSpec extends FunSuite {

  import dsl.uncached.justified._

  test("repeat[A].take(10)") {
    // TODO: The following use case should also be supported once .take is supported
    // repeat(1.const).take(10)
  }

  test("wrap(repeat[A], Seq[B]).zipToShortest produces a Seq[A :: B :: HNil]") {
    val ageFacts = valuesOfType(FactTypes.Age)
    val scoreFacts = valuesOfType(FactTypes.Scores)
    val expr = wrap(ageFacts, scoreFacts).zipToShortest.flatMap { hl =>
      val age = hl.get(_.at(Nat._0))
      val scores = hl.get(_.at(Nat._1))
      wrap(repeatConstForever(age), scores).zipToShortest
    }
    val ageFact = FactTypes.Age(30)
    val scoresFact = FactTypes.Scores(Seq(1.0, 2.0))
    val facts = FactTable(ageFact, scoresFact)
    val observed = expr.run(facts)
    val expected = Vector(
      Justified.byInference(
        "map",
        30 :: 1.0 :: HNil,
        NonEmptySeq.of(
          Justified.byInference(
            "product",
            (30, 1.0 :: HNil),
            NonEmptySeq.of(
              Justified.bySelection(
                30,
                DataPath.empty.atKey(0),
                Justified.ByInference(
                  "map",
                  30 :: List(1.0, 2.0) :: HNil,
                  NonEmptySeq.of(
                    Justified.byInference(
                      "product",
                      (30, List(1.0, 2.0) :: HNil),
                      NonEmptySeq.of(
                        Justified.byFact(ageFact),
                        Justified.byInference(
                          "map",
                          List(1.0, 2.0) :: HNil,
                          NonEmptySeq.of(
                            Justified.byFact(scoresFact),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
              Justified.byInference(
                "map",
                1.0 :: HNil,
                NonEmptySeq.of(
                  Justified.bySelection(
                    1.0,
                    DataPath.empty.atKey(1).atIndex(0),
                    Justified.byInference(
                      "map",
                      30 :: List(1.0, 2.0) :: HNil,
                      NonEmptySeq.of(
                        Justified.byInference(
                          "product",
                          (30, List(1.0, 2.0) :: HNil),
                          NonEmptySeq.of(
                            Justified.byFact(ageFact),
                            Justified
                              .byInference("map", List(1.0, 2.0) :: HNil, NonEmptySeq.of(Justified.byFact(scoresFact))),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
      Justified.byInference(
        "map",
        30 :: 2.0 :: HNil,
        NonEmptySeq.of(
          Justified.byInference(
            "product",
            (30, 2.0 :: HNil),
            NonEmptySeq.of(
              Justified.bySelection(
                30,
                DataPath.empty.atKey(0),
                Justified.byInference(
                  "map",
                  30 :: List(1.0, 2.0) :: HNil,
                  NonEmptySeq.of(
                    Justified.ByInference(
                      "product",
                      (30, List(1.0, 2.0) :: HNil),
                      NonEmptySeq.of(
                        Justified.byFact(ageFact),
                        Justified.byInference(
                          "map",
                          List(1.0, 2.0) :: HNil,
                          NonEmptySeq.of(
                            Justified.byFact(scoresFact),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
              Justified.ByInference(
                "map",
                2.0 :: HNil,
                NonEmptySeq.of(
                  Justified.bySelection(
                    2.0,
                    DataPath.empty.atKey(1).atIndex(1),
                    Justified.byInference(
                      "map",
                      30 :: List(1.0, 2.0) :: HNil,
                      NonEmptySeq.of(
                        Justified.ByInference(
                          "product",
                          (30, List(1.0, 2.0) :: HNil),
                          NonEmptySeq.of(
                            Justified.byFact(ageFact),
                            Justified.byInference(
                              "map",
                              List(1.0, 2.0) :: HNil,
                              NonEmptySeq.of(
                                Justified.byFact(scoresFact),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    )
    assertEquals(observed, expected)
  }
}
