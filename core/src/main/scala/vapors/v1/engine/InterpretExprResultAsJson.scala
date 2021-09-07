package com.rallyhealth

package vapors.v1.engine

import vapors.v1.algebra.ExprResult
import vapors.v1.data.ExprState

import cats.Foldable
import io.circe.syntax._
import io.circe.{Encoder, JsonObject}

object InterpretExprResultAsJson {
  type Serialize[-I, +O] = JsonObject

  final object Visitor {

    @inline def nothing: Visitor[Nothing] = new Visitor(None)

    @inline def unit: Visitor[Unit] = new Visitor(None)

    @inline def apply[PO, O](
      previousState: ExprState[PO, O],
    )(implicit
      encodeState: Encoder.AsObject[ExprState[PO, O]],
    ): Visitor[PO] = new Visitor[PO](Some(previousState.asJsonObject))
  }

  final class Visitor[-PO](prevStateJson: Option[JsonObject]) extends ExprResult.Visitor[PO, Serialize, Encoder] {

    private[this] implicit def encodeState[O : Encoder]: Encoder.AsObject[ExprState[PO, O]] =
      Encoder.AsObject.instance { state =>
        prevStateJson
          .getOrElse(JsonObject.empty)
          .add("output", state.output.asJson)
      }

    private[this] def encodeExprResult[O, OP[_]](
      result: ExprResult[PO, Nothing, O, OP],
    )(implicit
      encodeState: Encoder.AsObject[ExprState[PO, O]],
    ): JsonObject = {
      result.state.asJsonObject.deepMerge(
        JsonObject(
          "expr" -> result.expr.name.asJson,
        ),
      )
    }

    override def visitAndThen[AI, AO : Encoder, BI, BO : Encoder](
      result: ExprResult.AndThen[PO, AI, AO, BI, BO, Encoder],
    )(implicit
      ev: AO <:< BI,
    ): Serialize[AI, BO] = encodeExprResult(result)

    override def visitCombine[I, LI, LO : Encoder, RI, RO : Encoder, O : Encoder](
      result: ExprResult.Combine[PO, I, LI, LO, RI, RO, O, Encoder],
    ): Serialize[I, O] = {
      encodeExprResult(result)
        .add("operation", result.expr.operationName.asJson)
        .add("left", result.leftResult.visit(this).asJson)
        .add("right", result.rightResult.visit(this).asJson)
    }

    override def visitConst[O : Encoder](result: ExprResult.Const[PO, O, Encoder]): Serialize[Any, O] =
      encodeExprResult(result)

    override def visitExists[C[_] : Foldable, E](
      result: ExprResult.Exists[PO, C, E, Encoder],
    )(implicit
      opO: Encoder[Boolean],
    ): Serialize[C[E], Boolean] = encodeExprResult(result)

    override def visitForAll[C[_] : Foldable, E](
      result: ExprResult.ForAll[PO, C, E, Encoder],
    )(implicit
      opO: Encoder[Boolean],
    ): Serialize[C[E], Boolean] = encodeExprResult(result)

    override def visitIdentity[I, O : Encoder](
      result: ExprResult.Identity[PO, I, O, Encoder],
    )(implicit
      ev: I <:< O,
    ): Serialize[I, O] = encodeExprResult(result)

    override def visitValuesOfType[T](
      result: ExprResult.ValuesOfType[PO, T, Encoder],
    )(implicit
      opTs: Encoder[Seq[T]],
    ): Serialize[Any, Seq[T]] = encodeExprResult(result)
  }
}
