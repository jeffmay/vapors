package com.rallyhealth

package vapors.v1.engine

import vapors.v1.algebra.ExprResult
import vapors.v1.data.ExprState
import vapors.v1.dsl.circe._

import cats.Foldable
import io.circe.syntax._
import io.circe.{Encoder, JsonObject}

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

    @inline def apply[PO : Encoder]: Visitor[PO] = new Visitor
  }

  final class Visitor[-PO : Encoder] extends ExprResult.Visitor[PO, Serialize, Encoder] {

    override def visitCombine[I, LO : Encoder, RO : Encoder, LI, RI, O : Encoder](
      result: ExprResult.Combine[PO, I, LO, RO, LI, RI, O, Encoder],
    ): Serialize[I, O] = {
      encodeExprResult(result)
        .add("left", result.leftResult.visit(Visitor[PO]).asJson)
        .add("right", result.rightResult.visit(Visitor[PO]).asJson)
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
