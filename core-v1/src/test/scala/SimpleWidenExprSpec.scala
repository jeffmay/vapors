package com.rallyhealth.vapors.v1

import algebra.{CombineHolder, Expr, SelectHolder}

import munit.FunSuite

class SimpleWidenExprSpec extends FunSuite {

  import dsl.uncached._

  def assertTypeMismatch(
    expectedType: String,
    errors: String,
  ): Unit = {
    assertEquals(
      errors.linesWithSeparators.take(3).mkString,
      s"""error:
         |type mismatch;
         | found   : $expectedType
         |""".stripMargin,
    )
  }

  test("Const.simplify") {
    val orig: Expr.Const[Int, OP] = 1.const
    val simple = orig.widen
    assert(simple.isInstanceOf[Any ~:> Int])
    assertTypeMismatch(
      "Any ~:> Int",
      compileErrors {
        "simple: Expr.Const[Int, OP]"
      },
    )
  }

  test("Identity.simplify") {
    val orig: Expr.Identity[Int, OP] = ident[Int]
    val simple = orig.widen
    assert(simple.isInstanceOf[Int ~:> Int])
    assertTypeMismatch(
      "Int ~:> Int",
      compileErrors {
        "simple: Expr.Identity[Int, OP]"
      },
    )
  }

  test("Map.simplify") {
    val orig: Expr.AndThen[Any, Seq[Int], Seq[Int], Seq[Int], OP] = Seq(1).const.map(_ + 1.const)
    val simple = orig.widen
    assert(simple.isInstanceOf[Any ~:> Seq[Int]])
    assertTypeMismatch(
      "Any ~:> Seq[Int]",
      compileErrors {
        "simple: Expr.AndThen[Any, Seq[Int], Seq[Int], Seq[Int], OP]"
      },
    )
  }

  test("CombineHolder.simplify") {
    val orig: CombineHolder[Any, Int, Int, Int, Int, Int, OP] = 1.const + 1.const
    val simple = orig.widen
    assert(simple.isInstanceOf[Any ~:> Int])
    assertTypeMismatch(
      "Any ~:> Int",
      compileErrors {
        "simple: CombineHolder[Any, Int, Int, Int, Int, Int, OP]"
      },
    )
  }

  test("SelectHolder.simplify") {
    val orig: SelectHolder[Any, Vector[Int], List[Int], List[Int], OP] = Vector(2).const.to[List]
    val simple = orig.widen
    assert(simple.isInstanceOf[Any ~:> List[Int]])
    assertTypeMismatch(
      "Any ~:> List[Int]",
      compileErrors {
        "simple: SelectHolder[Any, Vector[Int], List[Int], List[Int], OP]"
      },
    )
  }
}
