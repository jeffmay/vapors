package com.rallyhealth.vapors.v1

package engine

import algebra.{EqualComparable, ExprResult, WindowComparable}
import cats.{Foldable, Functor, FunctorFilter}
import data.{ExprState, Extract}
import data.ExtractValue.AsBoolean
import debug.HasSourceCodeInfo
import dsl.circe.HasEncoder
import logic.{Conjunction, Disjunction, Negation}
import io.circe.syntax._
import io.circe.{Encoder, JsonObject}

import scala.annotation.nowarn

object InterpretExprResultAsJson {
  type ToJsonObject[-I, +O] = JsonObject

  final object Visitor {

    @inline def nothing[OP[a] <: HasEncoder[a]]: Visitor[Nothing, OP] = new Visitor(None)

    @inline def unit[OP[a] <: HasEncoder[a]]: Visitor[Unit, OP] = new Visitor(None)

    @inline def apply[OP[a] <: HasEncoder[a]]: WithParam[OP] = new WithParam

    final class WithParam[OP[a] <: HasEncoder[a]](@nowarn private val dummy: Boolean = true) extends AnyVal {

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

    override def visitAnd[I, B, W[+_]](
      result: ExprResult.And[PO, I, B, W, OP],
    )(implicit
      logic: Conjunction[W, B, OP],
      opO: OP[W[B]],
    ): ToJsonObject[I, W[B]] =
      encodeExprResult(result)

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

    override def visitCustomFunction[I, O : OP](result: ExprResult.CustomFunction[PO, I, O, OP]): ToJsonObject[I, O] =
      encodeExprResult(result)

    override def visitExists[C[_] : Foldable, A, B : AsBoolean : OP](
      result: ExprResult.Exists[PO, C, A, B, OP],
    ): ToJsonObject[C[A], B] = encodeExprResult(result)

    override def visitFilter[C[_] : FunctorFilter, A, B : AsBoolean : OP](
      result: ExprResult.Filter[PO, C, A, B, OP],
    )(implicit
      opO: OP[C[A]],
    ): ToJsonObject[C[A], C[A]] = encodeExprResult(result)

    override def visitForAll[C[_] : Foldable, A, B : AsBoolean : OP](
      result: ExprResult.ForAll[PO, C, A, B, OP],
    ): ToJsonObject[C[A], B] = encodeExprResult(result)

    override def visitIdentity[I : OP](result: ExprResult.Identity[PO, I, OP]): ToJsonObject[I, I] =
      encodeExprResult(result)

    override def visitIsEqual[I, V, W[+_]](
      result: ExprResult.IsEqual[PO, I, V, W, OP],
    )(implicit
      eq: EqualComparable[W, V, OP],
      opV: OP[W[V]],
      opO: OP[W[Boolean]],
    ): ToJsonObject[I, W[Boolean]] =
      encodeExprResult(result)

    override def visitMapEvery[C[_] : Functor, A, B](
      result: ExprResult.MapEvery[PO, C, A, B, OP],
    )(implicit
      opO: OP[C[B]],
    ): ToJsonObject[C[A], C[B]] =
      encodeExprResult(result)

    override def visitNot[I, B, W[+_]](
      result: ExprResult.Not[PO, I, B, W, OP],
    )(implicit
      logic: Negation[W, B, OP],
      opB: OP[W[B]],
    ): ToJsonObject[I, W[B]] = encodeExprResult(result)

    override def visitOr[I, B, W[+_]](
      result: ExprResult.Or[PO, I, B, W, OP],
    )(implicit
      logic: Disjunction[W, B, OP],
      opO: OP[W[B]],
    ): ToJsonObject[I, W[B]] =
      encodeExprResult(result)

    override def visitSelect[I, A, B, O : OP](result: ExprResult.Select[PO, I, A, B, O, OP]): ToJsonObject[I, O] =
      encodeExprResult(result)

    override def visitValuesOfType[T, O](
      result: ExprResult.ValuesOfType[PO, T, O, OP],
    )(implicit
      opTs: OP[Seq[O]],
    ): ToJsonObject[Any, Seq[O]] =
      encodeExprResult(result)
        .add("factTypes", result.expr.factTypeSet.typeList.toList.map(_.name).asJson)

    override def visitWithinWindow[I, V, W[+_]](
      result: ExprResult.WithinWindow[PO, I, V, W, OP],
    )(implicit
      comparison: WindowComparable[W, OP],
      opB: OP[W[Boolean]],
    ): ToJsonObject[I, W[Boolean]] = {
      encodeExprResult(result)
    }
  }

  final object DebugVisitor {

    @inline def apply[OP[a] <: HasEncoder[a] with HasSourceCodeInfo]: WithParam[OP] = new WithParam

    final class WithParam[OP[a] <: HasEncoder[a] with HasSourceCodeInfo](@nowarn private val dummy: Boolean = true)
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
      import dsl.circe.encoders.encodeHasSourceCodeInfo
      HasSourceCodeInfo.fromContext[OP, O].asJsonObject
    }

    override def visitAnd[I, B, W[+_]](
      result: ExprResult.And[PO, I, B, W, OP],
    )(implicit
      logic: Conjunction[W, B, OP],
      opO: OP[W[B]],
    ): ToJsonObject[I, W[B]] = super.visitAnd(result).deepMerge(sourceInfo[W[B]])

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

    override def visitCustomFunction[I, O : OP](result: ExprResult.CustomFunction[PO, I, O, OP]): ToJsonObject[I, O] =
      super.visitCustomFunction(result).deepMerge(sourceInfo[O])

    override def visitExists[C[_] : Foldable, A, B : AsBoolean : OP](
      result: ExprResult.Exists[PO, C, A, B, OP],
    ): ToJsonObject[C[A], B] = super.visitExists(result).deepMerge(sourceInfo[B])

    override def visitFilter[C[_] : FunctorFilter, A, B : AsBoolean : OP](
      result: ExprResult.Filter[PO, C, A, B, OP],
    )(implicit
      opO: OP[C[A]],
    ): ToJsonObject[C[A], C[A]] = super.visitFilter(result).deepMerge(sourceInfo[C[A]])

    override def visitForAll[C[_] : Foldable, A, B : AsBoolean : OP](
      result: ExprResult.ForAll[PO, C, A, B, OP],
    ): ToJsonObject[C[A], B] = super.visitForAll(result).deepMerge(sourceInfo[B])

    override def visitIdentity[I : OP](result: ExprResult.Identity[PO, I, OP]): ToJsonObject[I, I] =
      super.visitIdentity(result).deepMerge(sourceInfo[I])

    override def visitIsEqual[I, V, W[+_]](
      result: ExprResult.IsEqual[PO, I, V, W, OP],
    )(implicit
      eq: EqualComparable[W, V, OP],
      opV: OP[W[V]],
      opO: OP[W[Boolean]],
    ): ToJsonObject[I, W[Boolean]] = super.visitIsEqual(result).deepMerge(sourceInfo[W[Boolean]])

    override def visitMapEvery[C[_] : Functor, A, B](
      result: ExprResult.MapEvery[PO, C, A, B, OP],
    )(implicit
      opO: OP[C[B]],
    ): ToJsonObject[C[A], C[B]] = super.visitMapEvery(result).deepMerge(sourceInfo[C[B]])

    override def visitNot[I, B, W[+_]](
      result: ExprResult.Not[PO, I, B, W, OP],
    )(implicit
      logic: Negation[W, B, OP],
      opB: OP[W[B]],
    ): ToJsonObject[I, W[B]] = super.visitNot(result).deepMerge(sourceInfo[W[B]])

    override def visitOr[I, B, W[+_]](
      result: ExprResult.Or[PO, I, B, W, OP],
    )(implicit
      logic: Disjunction[W, B, OP],
      opO: OP[W[B]],
    ): ToJsonObject[I, W[B]] = super.visitOr(result).deepMerge(sourceInfo[W[B]])

    override def visitSelect[I, A, B, O : OP](result: ExprResult.Select[PO, I, A, B, O, OP]): ToJsonObject[I, O] =
      super.visitSelect(result).deepMerge(sourceInfo[O])

    override def visitValuesOfType[T, O](
      result: ExprResult.ValuesOfType[PO, T, O, OP],
    )(implicit
      opTs: OP[Seq[O]],
    ): ToJsonObject[Any, Seq[O]] = super.visitValuesOfType(result).deepMerge(sourceInfo[Seq[O]])

    override def visitWithinWindow[I, V, W[+_]](
      result: ExprResult.WithinWindow[PO, I, V, W, OP],
    )(implicit
      comparison: WindowComparable[W, OP],
      opB: OP[W[Boolean]],
    ): ToJsonObject[I, W[Boolean]] =
      super.visitWithinWindow(result).deepMerge(sourceInfo[W[Boolean]])
  }

}
