package com.rallyhealth.vapors.v1

package dsl

import data.RegexMatch

import shapeless.Id

import scala.util.matching.Regex

trait WrapRegexMatches[W[+_], OP[_]] {

  def wrapMatched[O](
    in: W[String],
    out: O,
    pattern: Regex,
    matches: LazyList[RegexMatch],
  ): W[O]
}

object WrapRegexMatches {

  @inline final def unwrapped[OP[_]]: WrapRegexMatches[Id, OP] = Unwrapped.asInstanceOf[WrapRegexMatches[Id, OP]]

  private final object Unwrapped extends WrapRegexMatches[Id, Any] {
    override def wrapMatched[O](
      in: String,
      out: O,
      pattern: Regex,
      matches: LazyList[RegexMatch],
    ): O = out
  }
}
