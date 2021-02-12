package com.rallyhealth.vapors.core

import cats.Id
import com.rallyhealth.vapors.core.algebra.{CaptureP, Expr}
import com.rallyhealth.vapors.core.data.{FactTable, TypedFact}

package object dsl extends ExprDsl with ExprBuilderSyntax with WithOutputSyntax with ExprBuilderCatsInstances {

  final type CondExpr[F[_], V, P] = Expr[F, V, Boolean, P]

  final type ValExpr[V, R, P] = Expr[Id, V, R, P]
  final type ValCondExpr[V, P] = ValExpr[V, Boolean, P]

  final type RootExpr[R, P] = Expr[Id, FactTable, R, P]

  final type CaptureRootExpr[R, P] = CaptureP[Id, FactTable, R, P]
  final type CaptureFromFacts[T, P] = CaptureP[Seq, TypedFact[T], Seq[TypedFact[T]], P]
}
