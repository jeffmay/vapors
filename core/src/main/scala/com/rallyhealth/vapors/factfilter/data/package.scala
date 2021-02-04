package com.rallyhealth.vapors.factfilter

import cats.Order
import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.data
import com.rallyhealth.vapors.core.lens

package object data {

  // Removed type aliases //

  @deprecated("Removed in next breaking version change", "0.8.0")
  final type Facts = NonEmptyList[data.Fact]

  @deprecated("Removed in next breaking version change", "0.8.0")
  final type FactLens[T, V] = lens.NamedLens[data.TypedFact[T], V]

  @deprecated("Removed in next breaking version change", "0.8.0")
  final type FactLensId[T] = lens.NamedLens.Id[data.TypedFact[T]]

  @deprecated("Removed in next breaking version change", "0.8.0")
  final type FactsOfType[T] = NonEmptyList[data.TypedFact[T]]

  @deprecated("Removed in next breaking version change", "0.8.0")
  final type TypedFacts[T] = List[data.TypedFact[T]]

  @deprecated("Removed in next breaking version change", "0.8.0")
  final type OrderTypedFacts[T] = Order[data.TypedFact[T]]

  // Aliases for source compatibility warnings //

  @deprecated("Use com.rallyhealth.vapors.core.data.DerivedFact instead", "0.8.0")
  final type DerivedFact = data.DerivedFact

  @deprecated("Use com.rallyhealth.vapors.core.data.DerivedFact instead", "0.8.0")
  final val DerivedFact = data.DerivedFact

  @deprecated("Use com.rallyhealth.vapors.core.data.DerivedFactOfType instead", "0.8.0")
  final type DerivedFactOfType[T] = data.DerivedFactOfType[T]

  @deprecated("Use com.rallyhealth.vapors.core.data.DerivedFactOfType instead", "0.8.0")
  final val DerivedFactOfType = data.DerivedFactOfType

  @deprecated("Use com.rallyhealth.vapors.core.data.SourceFactOfType instead", "0.8.0")
  final type SourceFactOfType[T] = data.SourceFactOfType[T]

  @deprecated("Use com.rallyhealth.vapors.core.data.SourceFactOfType instead", "0.8.0")
  final val SourceFactOfType = data.SourceFactOfType

  @deprecated("Use com.rallyhealth.vapors.core.data.FactSet instead", "0.8.0")
  final type FactSet = data.FactSet

  @deprecated("Use com.rallyhealth.vapors.core.data.FactSet instead", "0.8.0")
  final val FactSet = data.FactSet

  @deprecated("Use com.rallyhealth.vapors.core.data.TypedFact instead", "0.8.0")
  final type TypedFact[T] = data.TypedFact[T]

  @deprecated("Use com.rallyhealth.vapors.core.data.TypedFact instead", "0.8.0")
  final val TypedFact = data.TypedFact

  @deprecated("Use com.rallyhealth.vapors.core.data.TypedFactSet instead", "0.8.0")
  final type TypedFactSet[T] = data.TypedFactSet[T]

  @deprecated("Use com.rallyhealth.vapors.core.data.TypedFactSet instead", "0.8.0")
  final val TypedFactSet = data.TypedFactSet

  @deprecated("Use com.rallyhealth.vapors.core.data.Evidence instead", "0.8.0")
  final type Evidence = data.Evidence

  @deprecated("Use com.rallyhealth.vapors.core.data.Evidence instead", "0.8.0")
  final val Evidence = data.Evidence

  // TODO: Move somewhere else
  @deprecated("Use com.rallyhealth.vapors.core.data.ExtractBoolean instead", "0.8.0")
  final type ExtractBoolean[-T] = data.ExtractBoolean[T]

  // TODO: Move somewhere else
  @deprecated("Use com.rallyhealth.vapors.core.data.ExtractBoolean instead", "0.8.0")
  final val ExtractBoolean = data.ExtractBoolean

  // TODO: Move somewhere else
  @deprecated("Use com.rallyhealth.vapors.core.data.ExtractValue instead", "0.8.0")
  final type ExtractValue[-T, +V] = data.ExtractValue[T, V]

  // TODO: Move somewhere else
  @deprecated("Use com.rallyhealth.vapors.core.data.ExtractValue instead", "0.8.0")
  final val ExtractValue = data.ExtractValue

  @deprecated("Use com.rallyhealth.vapors.core.data.Fact instead", "0.8.0")
  final type Fact = data.Fact

  @deprecated("Use com.rallyhealth.vapors.core.data.Fact instead", "0.8.0")
  final val Fact = data.Fact

  @deprecated("Use com.rallyhealth.vapors.core.data.FactOrFactSet instead", "0.8.0")
  final type FactOrFactSet = data.FactOrFactSet

  @deprecated("Use com.rallyhealth.vapors.core.data.FactOrFactSet instead", "0.8.0")
  final val FactOrFactSet = data.FactOrFactSet

  @deprecated("Use com.rallyhealth.vapors.core.data.FactTable instead", "0.8.0")
  final type FactTable = data.FactTable

  @deprecated("Use com.rallyhealth.vapors.core.data.FactTable instead", "0.8.0")
  final val FactTable = data.FactTable

  @deprecated("Use com.rallyhealth.vapors.core.data.FactType instead", "0.8.0")
  final type FactType[T] = data.FactType[T]

  @deprecated("Use com.rallyhealth.vapors.core.data.FactType instead", "0.8.0")
  final val FactType = data.FactType

  @deprecated("Use com.rallyhealth.vapors.core.data.FactTypeSet instead", "0.8.0")
  final type FactTypeSet[T] = data.FactTypeSet[T]

  @deprecated("Use com.rallyhealth.vapors.core.data.FactTypeSet instead", "0.8.0")
  final val FactTypeSet = data.FactTypeSet

}
