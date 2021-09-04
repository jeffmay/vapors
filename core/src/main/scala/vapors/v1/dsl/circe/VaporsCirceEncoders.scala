package com.rallyhealth

package vapors.v1.dsl.circe

import vapors.v1.data.ExprState

import io.circe.syntax._
import io.circe.{Encoder, Json}

trait LowPriorityVaporsCirceEncoders {

  implicit def encodeExprState[I : Encoder, O : Encoder]: Encoder[ExprState[I, O]] = Encoder.instance { state =>
    Json.obj(
      "input" -> state.input.asJson,
      "output" -> state.output.asJson,
    )
  }
}

trait VaporsCirceEncoders extends LowPriorityVaporsCirceEncoders {

  implicit def encodeExprStateInputUnit[O : Encoder]: Encoder[ExprState[Unit, O]] = Encoder.instance { state =>
    Json.obj(
      "output" -> state.output.asJson,
    )
  }

  implicit def encodeExprStateInputNothing[O : Encoder]: Encoder[ExprState[Nothing, O]] = Encoder.instance { state =>
    Json.obj(
      "output" -> state.output.asJson,
    )
  }

  implicit def encodeExprStateOutputUnit[I : Encoder]: Encoder[ExprState[I, Unit]] = Encoder.instance { state =>
    Json.obj(
      "input" -> state.input.asJson,
    )
  }

  implicit def encodeExprStateOutputNothing[I : Encoder]: Encoder[ExprState[I, Nothing]] = Encoder.instance { state =>
    Json.obj(
      "input" -> state.input.asJson,
    )
  }
}
