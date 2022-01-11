package com.rallyhealth.vapors.v1

package lens

import cats.data.{NonEmptySeq, NonEmptyVector}
import munit.FunSuite

class VariantLensSpec extends FunSuite {

  test("VariantLens.asIterable[Seq].to[Vector]") {
    val expected: VariantLens[Seq[Int], Vector[Int]] = VariantLens.id[Seq[Int]].to[Vector]
  }

  test("VariantLens.asIterable[List].to[Seq]") {
    val expected: VariantLens[List[Int], Seq[Int]] = VariantLens.id[List[Int]].to[Seq]
  }

  test("VariantLens.asIterable[Option].to[Seq]") {
    val expected: VariantLens[Option[Int], Seq[Int]] = VariantLens.id[Option[Int]].to[Seq]
  }

  test("VariantLens.asFoldable[NonEmptySeq].to[List]") {
    val expected: VariantLens[NonEmptySeq[Int], Seq[Int]] = VariantLens.id[NonEmptySeq[Int]].to[List]
  }

  test("VariantLens.asFoldable[NonEmptyVector].to[Vector]") {
    val expected: VariantLens[NonEmptyVector[Int], Vector[Int]] = VariantLens.id[NonEmptyVector[Int]].to[Vector]
  }
}
