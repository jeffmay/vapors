package com.rallyhealth

package vapors.v1.dsl

import vapors.v1.algebra.ExprResult
import vapors.v1.engine.InterpretExprResultAsJson

import com.rallyhealth.vapors.v1.data.ExprState
import io.circe.{Encoder, Json}

package object circe extends CirceDsl with VaporsCirceEncoders {

  // TODO: Convert to implicit Encoder?
  def serialize[PO, O](
    expr: ExprResult[PO, Nothing, O, Encoder],
  )(implicit
    encodeState: Encoder[ExprState[PO, O]],
  ): Json = {
    Json.fromJsonObject(expr.visit(InterpretExprResultAsJson.Visitor(expr.state)))
  }
}
