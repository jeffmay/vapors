package com.rallyhealth.vapors.core

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

  // Aliases for source compatibility //

  @deprecated("Use com.rallyhealth.vapors.core.lens.DataPath instead.", "0.8.0")
  final type DataPath = lens.DataPath

  @deprecated("Use com.rallyhealth.vapors.core.lens.DataPath instead.", "0.8.0")
  final val DataPath = lens.DataPath

  @deprecated("Use com.rallyhealth.vapors.core.lens.Indexed instead.", "0.8.0")
  final type Indexed[C, K, V] = lens.Indexed[C, K, V]

  @deprecated("Use com.rallyhealth.vapors.core.lens.Indexed instead.", "0.8.0")
  final val Indexed = lens.Indexed

  @deprecated("Use com.rallyhealth.vapors.core.lens.NamedLens instead.", "0.8.0")
  final type NamedLens[A, B] = lens.NamedLens[A, B]

  @deprecated("Use com.rallyhealth.vapors.core.lens.NamedLens instead.", "0.8.0")
  final val NamedLens = lens.NamedLens

  @deprecated("Use com.rallyhealth.vapors.core.lens.ValidDataPathKey instead.", "0.8.0")
  final type ValidDataPathKey[K] = lens.ValidDataPathKey[K]

  @deprecated("Use com.rallyhealth.vapors.core.lens.ValidDataPathKey instead.", "0.8.0")
  final val ValidDataPathKey = lens.ValidDataPathKey

}
