package com.rallyhealth.vapors.core.interpreter

import com.rallyhealth.vapors.core.data.{Evidence, FactTable}

final case class ExprInput[V](
  value: V,
  evidence: Evidence,
  factTable: FactTable,
) {

  // TODO: Remove
  @inline def withFoldableValue[G[_], U](
    value: G[U],
    evidence: Evidence = this.evidence,
  ): ExprInput[G[U]] = copy(value = value, evidence = evidence)

  @inline def withValue[U](
    value: U,
    evidence: Evidence = this.evidence,
  ): ExprInput[U] = copy(value = value, evidence = evidence)
}

object ExprInput {

  type Init = ExprInput[FactTable]

  @inline def fromFactTable(factTable: FactTable): Init =
    ExprInput(factTable, Evidence.none, factTable)

  @inline def fromValue[V](
    value: V,
    evidence: Evidence,
    factTable: FactTable,
  ): ExprInput[V] =
    ExprInput(value, evidence, factTable)

  @inline def fromValue[V](
    value: V,
    evidence: Evidence = Evidence.none,
  ): ExprInput[V] =
    fromValue(value, evidence, FactTable(evidence.factSet))
}
