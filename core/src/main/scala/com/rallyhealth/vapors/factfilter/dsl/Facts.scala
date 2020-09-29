package com.rallyhealth.vapors.factfilter.dsl

import cats.data.NonEmptyList
import com.rallyhealth.vapors.factfilter.data.{Fact, Facts}

object Facts {

  def apply(
    head: Fact,
    tail: Fact*,
  ): Facts = NonEmptyList.of(head, tail: _*)
}
