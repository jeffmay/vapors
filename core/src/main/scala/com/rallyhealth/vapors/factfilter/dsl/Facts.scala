package com.rallyhealth.vapors.factfilter.dsl

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.data.Fact

@deprecated("Use FactSet instead.", "0.8.0")
object Facts {

  @deprecated("Use FactSet instead.", "0.8.0")
  def apply(
    head: Fact,
    tail: Fact*,
  ): NonEmptyList[Fact] = NonEmptyList.of(head, tail: _*)
}
