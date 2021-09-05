package com.rallyhealth

package vapors.v1.engine

import vapors.v1.algebra.ExprResult
import vapors.v1.data.ExprState

import cats.Foldable
import io.circe.syntax._
import io.circe.{Encoder, Json, JsonObject}

object InterpretExprResultAsJson {
  type Serialize[-I, +O] = JsonObject

  def encodeExprResult[PO, O, OP[_]](
    result: ExprResult[PO, Nothing, O, OP],
  )(implicit
    encodeState: Encoder[ExprState[PO, O]],
  ): JsonObject = {
    JsonObject(
      "expr" -> result.expr.name.asJson,
      "state" -> result.state.asJson,
    )
  }

  final object Visitor {

    @inline def nothing: Visitor[Nothing] = new Visitor(None)

    @inline def unit: Visitor[Unit] = new Visitor(None)

    @inline def apply[PO, O](
      previousState: ExprState[PO, O],
    )(implicit
      encodeState: Encoder[ExprState[PO, O]],
    ): Visitor[PO] = new Visitor[PO](Some(previousState.asJson))
  }

  final class Visitor[-PO](prevStateJson: Option[Json]) extends ExprResult.Visitor[PO, Serialize, Encoder] {

    private[this] implicit def encodeState[O : Encoder]: Encoder[ExprState[PO, O]] = Encoder.AsObject.instance {
      state =>
        prevStateJson
          .flatMap(_.asObject) // TODO: Use JsonObject in constructor
          .getOrElse(JsonObject.empty)
          .add("output", state.output.asJson)
    }

    override def visitCombine[I, LO : Encoder, RO : Encoder, LI, RI, O : Encoder](
      result: ExprResult.Combine[PO, I, LO, RO, LI, RI, O, Encoder],
    ): Serialize[I, O] = {
      encodeExprResult(result)
        .add("left", result.leftResult.visit(this).asJson)
        .add("right", result.rightResult.visit(this).asJson)
    }

    override def visitConst[O : Encoder](result: ExprResult.Const[PO, O, Encoder]): Serialize[Any, O] =
      encodeExprResult(result)

    override def visitExists[I, C[_] : Foldable, E](
      result: ExprResult.Exists[PO, I, C, E, Encoder],
    )(implicit
      opC: Encoder[C[E]],
      opO: Encoder[Boolean],
    ): Serialize[I, Boolean] = encodeExprResult(result)

    override def visitForAll[I, C[_] : Foldable, E](
      result: ExprResult.ForAll[PO, I, C, E, Encoder],
    )(implicit
      opC: Encoder[C[E]],
      opO: Encoder[Boolean],
    ): Serialize[I, Boolean] = encodeExprResult(result)

    override def visitIdentity[I, O : Encoder](
      result: ExprResult.Identity[PO, I, O, Encoder],
    )(implicit
      ev: I <:< O,
    ): Serialize[I, O] = encodeExprResult(result)

    override def visitWithFactValues[T, O : Encoder](
      result: ExprResult.WithFactValues[PO, T, O, Encoder],
    ): Serialize[Any, O] = encodeExprResult(result)
  }
}
