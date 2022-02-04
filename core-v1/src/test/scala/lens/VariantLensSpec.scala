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

  test("VariantLens[Person].asCase[Employee] returns Some[Employee] for an Employee") {
    val lens = VariantLens.id[Person].asCase[Employee]
    val expected = new Employee
    assertEquals(lens.get(expected), Some(expected))
  }

  test("VariantLens[Person].asCase[Employee] returns None for a Spouse") {
    val lens = VariantLens.id[Person].asCase[Employee]
    val expected = new Spouse
    assertEquals(lens.get(expected), None)
  }

  test("VariantLens[Person].asCase[Employee] doesn't compile") {
    compileErrors("VariantLens.id[Person].asCase[Employee]")
  }

  test("VariantLens[Seq[Person]].asCase[List[Employee]] returns Some[List[Employee]] for a List[Employee]") {
    val lens = VariantLens.id[Seq[Person]].asCase[List[Employee]]
    val expected = List(new Employee, new Employee)
    assertEquals(lens.get(expected), Some(expected))
  }

  test("VariantLens[Seq[Person]].asCase[List[Employee]] returns None for a List[Person]") {
    val lens = VariantLens.id[Seq[Person]].asCase[List[Employee]]
    val expected = List(new Employee, new Spouse)
    assertEquals(lens.get(expected), None)
  }
}

class Person
class Employee extends Person
class Spouse extends Person
