package com.rallyhealth.vapors.v1

package object data {

  /**
    * An ordered set of untyped [[Fact]]s.
    */
  final type FactSet = Set[Fact]

  /**
    * An ordered set of [[TypedFact]]s.
    *
    * @note not to be confused with a [[FactTypeSet]] (which is a set of [[FactType]]s, with not values)
    */
  final type TypedFactSet[T] = Set[TypedFact[T]]
}
