package com.rallyhealth.vapors.core.data

sealed abstract class Fact {
  type Value

  val typeInfo: FactType[Value]
  val value: Value
}

final case class TypedFact[A](
  typeInfo: FactType[A],
  value: A,
) extends Fact {
  type Value = A
}

object TypedFact {

  final def lens[A]: NamedLens.Id[TypedFact[A]] = NamedLens.id[TypedFact[A]]

  final def value[A]: NamedLens[TypedFact[A], A] = {
    NamedLens[TypedFact[A], A](DataPath.empty.atField("value"), _.value)
  }
}
