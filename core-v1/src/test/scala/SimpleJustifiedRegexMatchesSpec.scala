package com.rallyhealth.vapors.v1

import data.{Justified, RegexMatch, SliceRange}
import lens.DataPath

import cats.data.NonEmptySeq
import munit.FunSuite

class SimpleJustifiedRegexMatchesSpec extends FunSuite {

  import dsl.uncached.justified._

  test("matchesRegex returns true with an exact string match") {
    val exact = "exact"
    val re = exact.r
    val expr = exact.const.matchesRegex(re)
    val obtained = expr.run()
    val expected = Justified.byInference(
      "regex_matches",
      true,
      NonEmptySeq.of(Justified.byConst(exact), Justified.byConst(re)),
    )
    assertEquals(obtained, expected)
  }

  test("matchesRegex returns false") {
    val input = "correct"
    val re = "wrong".r
    val expr = input.const.matchesRegex(re)
    val obtained = expr.run()
    val expected = Justified.byInference(
      "regex_matches",
      false,
      NonEmptySeq.of(Justified.byConst(input), Justified.byConst(re)),
    )
    assertEquals(obtained, expected)
  }

  test("matchesRegex returns true for multiple substrings") {
    val input = "one match, two match, three match, more"
    val re = "match".r
    val expr = input.const.matchesRegex(re)
    val obtained = expr.run()
    val expected = Justified.byInference(
      "regex_matches",
      true,
      NonEmptySeq.of(Justified.byConst(input), Justified.byConst(re)),
    )
    assertEquals(obtained, expected)
  }

  test("findFirstMatch returns some exact match") {
    val exact = "exact"
    val re = exact.r
    val expr = exact.const.findFirstMatch(re)
    val obtained = expr.run()
    val firstMatch = RegexMatch(exact, SliceRange.Absolute(0, exact.length), Map())
    val expected = Justified.bySelection(
      firstMatch,
      DataPath.empty.atIndex(0),
      Justified.byInference(
        "regex_matches",
        LazyList(firstMatch),
        NonEmptySeq.of(Justified.byConst(exact), Justified.byConst(re)),
      ),
    )
    assertEquals(obtained, Some(expected))
  }

  test("findFirstMatch returns multiple matches") {
    val input = "one match, two match, three match, more"
    val exact = "match"
    val re = exact.r
    val expr = input.const.findFirstMatch(re)
    val obtained = expr.run()
    def matchAfter(prefix: String): RegexMatch =
      RegexMatch(exact, SliceRange.Absolute(prefix.length, prefix.length + exact.length), Map())

    val firstMatch = matchAfter("one ")
    val expected = Justified.BySelection(
      firstMatch,
      DataPath.empty.atIndex(0),
      Justified.byInference(
        "regex_matches",
        LazyList(
          firstMatch,
          matchAfter("one match, two "),
          matchAfter("one match, two match, three "),
        ),
        NonEmptySeq.of(Justified.byConst(input), Justified.byConst(re)),
      ),
    )
    assertEquals(obtained, Some(expected))
  }

  test("findFirstMatch returns none") {
    val expr = "correct".const.findFirstMatch("wrong".r)
    val obtained = expr.run()
    assertEquals(obtained, None)
  }

  test("findAllMatches returns one exact match") {
    val exact = "exact"
    val re = exact.r
    val expr = exact.const.findAllMatches(re)
    val obtained = expr.run()
    val firstMatch = RegexMatch(exact, SliceRange.Absolute(0, exact.length), Map())
    val expected = Justified.elements(
      Justified.byInference(
        "regex_matches",
        LazyList(firstMatch),
        NonEmptySeq.of(Justified.byConst(exact), Justified.byConst(re)),
      ),
    )
    assertEquals(obtained, expected)
  }

  test("findAllMatches returns multiple matches") {
    val input = "one match, two match, three match, more"
    val exact = "match"
    val re = exact.r
    val expr = input.const.findAllMatches(re)
    val obtained = expr.run()
    def matchAfter(prefix: String): RegexMatch =
      RegexMatch(exact, SliceRange.Absolute(prefix.length, prefix.length + exact.length), Map())

    val firstMatch = matchAfter("one ")
    val expected = Justified.elements(
      Justified.byInference(
        "regex_matches",
        LazyList(
          firstMatch,
          matchAfter("one match, two "),
          matchAfter("one match, two match, three "),
        ),
        NonEmptySeq.of(Justified.byConst(input), Justified.byConst(re)),
      ),
    )
    assertEquals(obtained, expected)
  }

  test("findAllMatches returns none") {
    val expr = "correct".const.findAllMatches("wrong".r)
    val obtained = expr.run()
    assertEquals(obtained, LazyList())
  }
}
