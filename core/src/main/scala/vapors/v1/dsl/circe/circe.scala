package com.rallyhealth

package vapors.v1.dsl

import vapors.v1.algebra.ExprResult
import vapors.v1.engine.InterpretExprResultAsJson

import io.circe.{Encoder, Json}

package object circe extends CirceDsl with VaporsCirceEncoders {

  // TODO: Convert to implicit Encoder?
  def serialize[PO : Encoder, O : Encoder](expr: ExprResult[PO, Nothing, O, Encoder]): Json = {
    Json.fromJsonObject(expr.visit(InterpretExprResultAsJson.Visitor[PO]))
  }
}
