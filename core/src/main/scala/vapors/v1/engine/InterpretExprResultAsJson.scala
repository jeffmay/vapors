package com.rallyhealth

package vapors.v1.engine

import vapors.v1.algebra.ExprResult
import vapors.v1.data.ExprState
import vapors.v1.debug.HasSourceCodeInfo
import vapors.v1.dsl.circe.HasEncoder

import cats.Foldable
import io.circe.syntax._
import io.circe.{Encoder, Json, JsonObject}

object InterpretExprResultAsJson {
  type ToJsonObject[-I, +O] = JsonObject

  final object Visitor {

    @inline def nothing[OP[a] <: HasEncoder[a]]: Visitor[Nothing, OP] = new Visitor(None)

    @inline def unit[OP[a] <: HasEncoder[a]]: Visitor[Unit, OP] = new Visitor(None)

    @inline def apply[OP[a] <: HasEncoder[a]]: WithParam[OP] = new WithParam

    final class WithParam[OP[a] <: HasEncoder[a]](private val dummy: Boolean = true) extends AnyVal {

      def apply[PO, O](
        previousState: ExprState[PO, O],
      )(implicit
        encodeState: Encoder.AsObject[ExprState[PO, O]],
      ): Visitor[PO, OP] = new Visitor[PO, OP](Some(previousState.asJsonObject))
    }
  }

  class Visitor[-PO, OP[a] <: HasEncoder[a]](prevStateJson: Option[JsonObject])
    extends ExprResult.Visitor[PO, ToJsonObject, OP] {

    private[this] implicit def encodeState[O : OP]: Encoder.AsObject[ExprState[PO, O]] = {
      Encoder.AsObject.instance { state =>
        prevStateJson
          .getOrElse(JsonObject.empty)
          .add("output", HasEncoder[O].encodeOutput(state.output))
      }
    }

    private[this] def encodeExprResult[O](
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

    override def visitAndThen[II, IO : OP, OI, OO : OP](
      result: ExprResult.AndThen[PO, II, IO, OI, OO, OP],
    )(implicit
      ev: IO <:< OI,
    ): ToJsonObject[II, OO] = {
      val inputResultJson = result.inputResult.visit(this)
      // take the output from the input result and pass it as the input to the output response encoder
      val outputResultJson = result.outputResult.visit(new Visitor[IO, OP](Some(inputResultJson)))
      encodeExprResult(result)
        .add("input", inputResultJson.asJson)
        .add("output", outputResultJson.asJson)
    }

    override def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      result: ExprResult.Combine[PO, I, LI, LO, RI, RO, O, OP],
    ): ToJsonObject[I, O] = {
      encodeExprResult(result)
        .add("operation", result.expr.operationName.asJson)
        .add("left", result.leftResult.visit(this).asJson)
        .add("right", result.rightResult.visit(this).asJson)
    }

    override def visitConst[O : OP](result: ExprResult.Const[PO, O, OP]): ToJsonObject[Any, O] =
      encodeExprResult(result)

    override def visitExists[C[_] : Foldable, E](
      result: ExprResult.Exists[PO, C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): ToJsonObject[C[E], Boolean] = encodeExprResult(result)

    override def visitForAll[C[_] : Foldable, E](
      result: ExprResult.ForAll[PO, C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): ToJsonObject[C[E], Boolean] = encodeExprResult(result)

    override def visitIdentity[I, O : OP](
      result: ExprResult.Identity[PO, I, O, OP],
    )(implicit
      ev: I <:< O,
    ): ToJsonObject[I, O] = encodeExprResult(result)

    override def visitValuesOfType[T](
      result: ExprResult.ValuesOfType[PO, T, OP],
    )(implicit
      opTs: OP[Seq[T]],
    ): ToJsonObject[Any, Seq[T]] =
      encodeExprResult(result)
        .add("factTypes", result.expr.factTypeSet.typeList.toList.map(_.name).asJson)
  }

  final object DebugVisitor {

    @inline def apply[OP[a] <: HasEncoder[a] with HasSourceCodeInfo]: WithParam[OP] = new WithParam

    final class WithParam[OP[a] <: HasEncoder[a] with HasSourceCodeInfo](private val dummy: Boolean = true)
      extends AnyVal {

      def apply[PO, O](
        previousState: ExprState[PO, O],
      )(implicit
        encodeState: Encoder.AsObject[ExprState[PO, O]],
      ): DebugVisitor[PO, OP] = new DebugVisitor[PO, OP](Some(previousState.asJsonObject))
    }
  }

  class DebugVisitor[-PO, OP[a] <: HasEncoder[a] with HasSourceCodeInfo](prevStateJson: Option[JsonObject])
    extends Visitor[PO, OP](prevStateJson) {

    private[this] def sourceInfo[O : OP]: JsonObject = {
      import vapors.v1.dsl.circe._
      HasSourceCodeInfo.fromContext[OP, O].asJsonObject
    }

    override def visitAndThen[II, IO : OP, OI, OO : OP](
      result: ExprResult.AndThen[PO, II, IO, OI, OO, OP],
    )(implicit
      ev: IO <:< OI,
    ): ToJsonObject[II, OO] = {
      val outputJson = JsonObject("output" -> sourceInfo[OO].asJson)
      val inputJson = JsonObject("input" -> sourceInfo[IO].asJson)
      val resultJson = super.visitAndThen(result).deepMerge(sourceInfo[OO])
      outputJson.deepMerge(inputJson).deepMerge(resultJson)
    }

    override def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      result: ExprResult.Combine[PO, I, LI, LO, RI, RO, O, OP],
    ): ToJsonObject[I, O] = {
      val leftJson = JsonObject("left" -> sourceInfo[LO].asJson)
      val rightJson = JsonObject("right" -> sourceInfo[RO].asJson)
      val resultJson = super.visitCombine(result).deepMerge(sourceInfo[O])
      rightJson.deepMerge(leftJson).deepMerge(resultJson)
    }

    override def visitConst[O : OP](result: ExprResult.Const[PO, O, OP]): ToJsonObject[Any, O] =
      super.visitConst(result).deepMerge(sourceInfo[O])

    override def visitExists[C[_] : Foldable, E](
      result: ExprResult.Exists[PO, C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): ToJsonObject[C[E], Boolean] = super.visitExists(result).deepMerge(sourceInfo[Boolean])

    override def visitForAll[C[_] : Foldable, E](
      result: ExprResult.ForAll[PO, C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): ToJsonObject[C[E], Boolean] = super.visitForAll(result).deepMerge(sourceInfo[Boolean])

    override def visitIdentity[I, O : OP](
      result: ExprResult.Identity[PO, I, O, OP],
    )(implicit
      ev: I <:< O,
    ): ToJsonObject[I, O] = super.visitIdentity(result).deepMerge(sourceInfo[O])

    override def visitValuesOfType[T](
      result: ExprResult.ValuesOfType[PO, T, OP],
    )(implicit
      opTs: OP[Seq[T]],
    ): ToJsonObject[Any, Seq[T]] = super.visitValuesOfType(result).deepMerge(sourceInfo[Seq[T]])
  }

}
