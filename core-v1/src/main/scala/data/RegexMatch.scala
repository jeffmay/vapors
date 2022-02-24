package com.rallyhealth.vapors.v1

package data

import scala.util.matching.Regex

/**
  * Represents the data provided from a regular expression matched within some input string.
  *
  * @see [[Regex.Match]]
  *
  * @param matched the string that was matched in the original input
  * @param slice the slice range of the matched string
  * @param groups a map of group names AND group indexes to their matched strings
  */
final case class RegexMatch(
  matched: String,
  slice: SliceRange.Absolute,
  groups: Map[String, String],
)

object RegexMatch {

  def unapply(m: RegexMatch): Some[(String, Map[String, String])] = Some((m.matched, m.groups))
  def unapply(m: Regex.Match): Some[(String, Map[String, String])] = unapply(from(m))

  def from(m: Regex.Match): RegexMatch =
    RegexMatch(
      m.matched,
      SliceRange.Absolute(m.start, m.end),
      (1 to m.groupCount).map(_.toString).zip(m.subgroups).toMap.withDefault { k =>
        Option(m.group(k)).getOrElse("")
      },
    )
}
