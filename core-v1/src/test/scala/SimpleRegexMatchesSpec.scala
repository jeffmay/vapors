package com.rallyhealth.vapors.v1

import data.{RegexMatch, SliceRange}

import munit.FunSuite

class SimpleRegexMatchesSpec extends FunSuite {

  import dsl.uncached._

  test("matchesRegex returns true with an exact string match") {
    val input = "exact"
    val expr = input.const.matchesRegex(input.r)
    val obtained = expr.run()
    assertEquals(obtained, true)
  }

  test("matchesRegex returns false") {
    val expr = "correct".const.matchesRegex("wrong".r)
    val obtained = expr.run()
    assertEquals(obtained, false)
  }

  test("matchesRegex returns true for multiple substrings") {
    val expr = "one match, two match, three match, more".const.matchesRegex("match".r)
    val obtained = expr.run()
    assertEquals(obtained, true)
  }

  test("findFirstMatch returns some exact match") {
    val exact = "exact"
    val re = exact.r
    val expr = exact.const.findFirstMatch(re)
    val obtained = expr.run()
    assertEquals(obtained, Some(RegexMatch(exact, SliceRange.Absolute(0, exact.length), Map())))
  }

  test("findFirstMatch returns multiple matches") {
    val input = "one match, two match, three match, more"
    val exact = "match"
    val re = exact.r
    val expr = input.const.findFirstMatch(re)
    val obtained = expr.run()
    val firstIdx = "one ".length
    assertEquals(obtained, Some(RegexMatch(exact, SliceRange.Absolute(firstIdx, firstIdx + exact.length), Map())))
  }

  test("findFirstMatch returns none") {
    val expr = "correct".const.findFirstMatch("wrong".r)
    val obtained = expr.run()
    assertEquals(obtained, None)
  }

  test("findAllMatches returns some exact match") {
    val exact = "exact"
    val re = exact.r
    val expr = exact.const.findAllMatches(re)
    val obtained = expr.run()
    assertEquals(obtained, LazyList(RegexMatch(exact, SliceRange.Absolute(0, exact.length), Map())))
  }

  test("findAllMatches returns multiple matches") {
    val input = "one match, two match, three match, more"
    val exact = "match"
    val re = exact.r
    val expr = input.const.findAllMatches(re)
    val obtained = expr.run()
    def matchAfter(prefix: String): RegexMatch =
      RegexMatch(exact, SliceRange.Absolute(prefix.length, prefix.length + exact.length), Map())

    val expected = LazyList(
      matchAfter("one "),
      matchAfter("one match, two "),
      matchAfter("one match, two match, three "),
    )
    assertEquals(obtained, expected)
  }

  test("findAllMatches returns empty") {
    val expr = "correct".const.findAllMatches("wrong".r)
    val obtained = expr.run()
    assertEquals(obtained, LazyList())
  }

}
