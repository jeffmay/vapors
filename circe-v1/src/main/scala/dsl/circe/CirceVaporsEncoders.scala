package com.rallyhealth.vapors.v1

package dsl.circe

import algebra.ExprResult
import data.ExprState
import debug.{HasSourceCodeInfo, SourceCodeInfo}
import engine.InterpretExprResultAsJson

import io.circe.syntax._
import io.circe.{Encoder, JsonObject}
import sourcecode.{File, Line}

trait CirceVaporsEncoders extends MidPrioritySourceInfoEncoders {

  implicit val encodeHasSourceCodeInfo: Encoder.AsObject[HasSourceCodeInfo] = Encoder.AsObject.instance { ctx =>
    val SourceCodeInfo(File(file), Line(line)) = ctx.debugSource
    JsonObject(
      "source" -> s"$file:$line".asJson,
    )
  }

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

/**
  * If you have a debugging result, we should encode the debugging details before falling back on the simple
  * result encoders from [[LowPrioritySimpleResultEncoders]].
  */
trait MidPrioritySourceInfoEncoders extends LowPrioritySimpleResultEncoders {

  implicit def encodeDebugExprResultWithDebugInfo[PO, I, O, OP[a] <: HasEncoder[a] with HasSourceCodeInfo](
    implicit
    encodeState: Encoder.AsObject[ExprState[PO, O]],
  ): Encoder.AsObject[ExprResult[PO, I, O, OP]] = Encoder.AsObject.instance { result =>
    result.visit(InterpretExprResultAsJson.DebugVisitor[OP](result.state))
  }

  implicit def encodeExprResultNoInputWithDebugInfo[
    O : OP,
    OP[a] <: HasEncoder[a] with HasSourceCodeInfo,
  ]: Encoder.AsObject[ExprResult[Nothing, Nothing, O, OP]] = {
    import encoders.encodeExprStateNoInput
    encodeDebugExprResultWithDebugInfo[Nothing, Nothing, O, OP]
  }
}

/**
  * When you don't have a debugging result, we should encode the expression results without them.
  */
trait LowPrioritySimpleResultEncoders {

  implicit def encodeExprResult[PO, I, O, OP[a] <: HasEncoder[a]](
    implicit
    encodeState: Encoder.AsObject[ExprState[PO, O]],
  ): Encoder.AsObject[ExprResult[PO, I, O, OP]] = Encoder.AsObject.instance { result =>
    result.visit(InterpretExprResultAsJson.Visitor[OP](result.state))
  }

  implicit def encodeExprResultNoInput[
    O : OP,
    OP[a] <: HasEncoder[a],
  ]: Encoder.AsObject[ExprResult[Nothing, Nothing, O, OP]] = {
    import encoders.encodeExprStateNoInput
    encodeExprResult[Nothing, Nothing, O, OP]
  }

  implicit def encodeOutput[O : HasEncoder]: Encoder[O] = HasEncoder[O].encodeOutput
}
