package com.rallyhealth.vapors.core.dsl

import cats.{Foldable, Id}
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.{FactSet, FactType}

final class DefinitionExprBuilder[T](private val factType: FactType[T]) extends AnyVal {

  def from[P](
    defExpr: RootExpr[T, P],
  )(implicit
    captureResult: CaptureRootExpr[FactSet, P],
  ): Expr.Define[Id, T, P] =
    Expr.Define[Id, T, P](factType, defExpr, captureResult)

  def fromEvery[M[_] : Foldable, P](
    defExpr: RootExpr[M[T], P],
  )(implicit
    captureResult: CaptureRootExpr[FactSet, P],
  ): Expr.Define[M, T, P] =
    Expr.Define(factType, defExpr, captureResult)
}
