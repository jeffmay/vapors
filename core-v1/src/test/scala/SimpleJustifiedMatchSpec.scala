package com.rallyhealth.vapors.v1

import data.{FactTable, Justified}
import example._

import munit.FunSuite

class SimpleJustifiedMatchSpec extends FunSuite {

  import dsl.uncached.justified._

  private val user = new User("1")
  private val subscriber = new Subscriber("2")
  private val member = new Member("3", "member")
  private val author = new Author("4", "author", 4)
  private val editor = new Editor("5", "editor", 5)
  private val admin = new Admin("6", "admin", 6)
  private val all = Seq(admin, editor, author, member, subscriber, user).map(FactTypes.RoleClass)

  test("Expr.Match matches subtype") {
    val expr = valuesOfType(FactTypes.RoleClass).filter {
      _.matching(
        Case[Author] ==> true.const,
      ).getOrElse(false.const)
    }
    val matched = expr.run(FactTable(all))
    val expected = Seq(admin, editor, author).map(v => Justified.byFact(FactTypes.RoleClass(v)))
    assertEquals(matched, expected)
  }

  test("Expr.Match matches subtype in sequence") {
    val expr = valuesOfType(FactTypes.RoleClass).filter {
      _.matching(
        Case[Admin] ==> true.const,
        Case[Editor] ==> false.const,
        Case[Author] ==> true.const,
      ).getOrElse(false.const)
    }
    val matched = expr.run(FactTable(all))
    val expected = Seq(admin, author).map(v => Justified.byFact(FactTypes.RoleClass(v)))
    assertEquals(matched, expected)
  }

  test("Expr.Match only evaluates the expression when the guard expression returns true") {
    val expr = valuesOfType(FactTypes.RoleClass).filter {
      _.matching(
        Case[Author].when(_.get(_.select(_.articles)) < 5.const) ==> false.const,
        Case[Author].when(_.get(_.select(_.articles)) > 4.const) ==> true.const,
      ).getOrElse(false.const)
    }
    val matched = expr.run(FactTable(all))
    val expected = Seq(admin, editor).map(v => Justified.byFact(FactTypes.RoleClass(v)))
    assertEquals(matched, expected)
  }

  test("Expr.Match only evaluates the expression when the guard expression returns true") {
    val expr = valuesOfType(FactTypes.RoleClass).filter {
      _.matching(
        Case[Author].when(_.get(_.select(_.articles)) > 5.const) ==> false.const,
        Case[Author].when(_.get(_.select(_.articles)) > 4.const) ==> true.const,
      ).getOrElse(false.const)
    }
    val matched = expr.run(FactTable(all))
    val expected = Seq(editor).map(v => Justified.byFact(FactTypes.RoleClass(v)))
    assertEquals(matched, expected)
  }
}
