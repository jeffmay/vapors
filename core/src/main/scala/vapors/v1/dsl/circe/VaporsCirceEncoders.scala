package com.rallyhealth

package vapors.v1.dsl.circe

import vapors.v1.algebra.ExprResult
import vapors.v1.data.ExprState
import vapors.v1.engine.InterpretExprResultAsJson

import io.circe.syntax._
import io.circe.{Encoder, JsonObject}

trait VaporsCirceEncoders {

  // TODO: Support other OP[_] types that include or extend an Encoder
  implicit def encodeExprResult[PO, I, O](
    implicit
    encodeState: Encoder.AsObject[ExprState[PO, O]],
  ): Encoder.AsObject[ExprResult[PO, I, O, Encoder]] = Encoder.AsObject.instance { result =>
    result.visit(InterpretExprResultAsJson.Visitor(result.state))
  }

  implicit def encodeExprResultNoInput[O : Encoder]: Encoder.AsObject[ExprResult[Nothing, Nothing, O, Encoder]] =
    encodeExprResult[Nothing, Nothing, O]

  implicit def encodeExprState[I : Encoder, O : Encoder]: Encoder.AsObject[ExprState[I, O]] =
    Encoder.AsObject.instance { state =>
      JsonObject(
        "input" -> state.input.asJson,
        "output" -> state.output.asJson,
      )
    }

  implicit val encoderExprStateEmpty: Encoder.AsObject[ExprState[Nothing, Nothing]] =
    Encoder.AsObject.instance(_ => JsonObject.empty)

  implicit def encodeExprStateNoInput[O : Encoder]: Encoder.AsObject[ExprState[Nothing, O]] =
    Encoder.AsObject.instance { state =>
      JsonObject(
        "output" -> state.output.asJson,
      )
    }

  implicit def encodeExprStateNoOutput[I : Encoder]: Encoder.AsObject[ExprState[I, Nothing]] =
    Encoder.AsObject.instance { state =>
      JsonObject(
        "input" -> state.input.asJson,
      )
    }

}
