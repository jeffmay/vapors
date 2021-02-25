package com.rallyhealth.vapors.core.dsl

import cats.Id
import com.rallyhealth.vapors.core.algebra.{CaptureP, Expr}
import com.rallyhealth.vapors.core.lens.{DataPath, NamedLens}
import shapeless.Nat

import scala.collection.{MapView, View}

final class MapViewExprBuilder[V, K, U, P](private val inputExpr: Expr[V, MapView[K, U], P]) extends AnyVal {

  def mapValues[R](
    buildExpr: ValExprBuilder[(K, U), U, P] => ExprBuilder[(K, U), Id, R, P],
  )(implicit
    captureView: CaptureP[V, View[(K, U)], P],
    captureEachTuple: CaptureP[(K, U), (K, U), P],
    captureEachTupleKey: CaptureP[(K, U), K, P],
    captureEachTupleValue: CaptureP[(K, U), U, P],
    captureEachResultTuple: CaptureP[(K, U), (K, R), P],
    captureViewResult: CaptureP[V, View[(K, R)], P],
    captureMapViewResult: CaptureP[V, MapView[K, R], P],
  ): MapViewExprBuilder[V, K, R, P] =
    MapViewExprBuilder
      .asFoldableBuilder(this)
      .map { kv =>
        val selectKeyFromTuple = kv.get(_.copy(path = DataPath.empty.atKey(Nat._1), get = _._1))
        val selectValueFromTuple = kv.get(_.copy(path = DataPath.empty.atKey(Nat._2), get = _._2))
        val mappedValue = buildExpr(new ValExprBuilder(selectValueFromTuple))
        wrap(
          selectKeyFromTuple.returnOutput,
          mappedValue.returnOutput,
        ).asTuple[(K, R)]
      }
      .toMap

  def values(
    implicit
    captureView: CaptureP[V, View[(K, U)], P],
    captureEachTuple: CaptureP[(K, U), (K, U), P],
    captureEachTupleValue: CaptureP[(K, U), U, P],
    captureResult: CaptureP[V, View[U], P],
  ): FoldableExprBuilder[V, View, U, P] = {
    new FoldableExprBuilder(
      Expr.MapOutput(
        Expr.SelectFromOutput(
          inputExpr,
          NamedLens.id[MapView[K, U]].asIterable.to(View),
          captureView,
        ),
        Expr.SelectFromOutput(
          Expr.ReturnInput(captureEachTuple),
          NamedLens[(K, U), U](DataPath.empty.atKey(Nat._2), _._2),
          captureEachTupleValue,
        ),
        captureResult,
      ),
    )
  }
}

object MapViewExprBuilder {

  implicit def asExpr[V, K, U, P](builder: MapViewExprBuilder[V, K, U, P]): Expr[V, MapView[K, U], P] =
    builder.inputExpr

  implicit def asFoldableBuilder[V, K, U, P](
    builder: MapViewExprBuilder[V, K, U, P],
  )(implicit
    captureView: CaptureP[V, View[(K, U)], P],
  ): FoldableExprBuilder[V, View, (K, U), P] =
    new FoldableExprBuilder(
      Expr.SelectFromOutput(
        builder.inputExpr,
        NamedLens.id[MapView[K, U]].asIterable.to(View),
        captureView,
      ),
    )
}
