package com.rallyhealth.vapors.core.interpreter

import cats.Id
import com.rallyhealth.vapors.core.data.{Evidence, FactTable}

final case class ExprInput[F[_], V](
  value: F[V],
  evidence: Evidence,
  factTable: FactTable,
) {

  @inline def withFoldableValue[G[_], U](
    value: G[U],
    evidence: Evidence = this.evidence,
  ): ExprInput[G, U] = copy(value = value, evidence = evidence)

  @inline def withValue[U](
    value: U,
    evidence: Evidence = this.evidence,
  ): ExprInput[Id, U] = copy[Id, U](value = value, evidence = evidence)
}

object ExprInput {

  type Init = ExprInput[Id, FactTable]

  @inline def fromFactTable(factTable: FactTable): Init =
    ExprInput[Id, FactTable](factTable, Evidence.none, factTable)

  @inline def fromValue[V](
    value: V,
    evidence: Evidence,
    factTable: FactTable,
  ): ExprInput[Id, V] =
    ExprInput[Id, V](value, evidence, factTable)

  @inline def fromValue[V](
    value: V,
    evidence: Evidence = Evidence.none,
  ): ExprInput[Id, V] =
    fromValue(value, evidence, FactTable(evidence.factSet))
}
