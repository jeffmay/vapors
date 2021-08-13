package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl.RootExpr

import cats.Id
import cats.effect.IO
import cats.effect.unsafe.IORuntime

sealed trait VaporsEngine[F[_], P] {
  type Result[R] <: VaporsResult[F, R, P]
  type ExtractParam

  def eval[R](
    rootExpr: RootExpr[R, P],
    factTable: FactTable = FactTable.empty,
  ): Result[R]

  final def evalAndExtractValue[R](
    rootExpr: RootExpr[R, P],
    factTable: FactTable = FactTable.empty,
  )(implicit
    param: ExtractParam,
  ): R =
    extract(eval(rootExpr, factTable).value)

  def extract[R](wrapped: F[R])(implicit param: ExtractParam): R
}

object StandardVaporsEngine extends StandardVaporsEngine[Unit] {
  @inline final def apply[P]: StandardVaporsEngine[P] = this.asInstanceOf[StandardVaporsEngine[P]]
}

sealed class StandardVaporsEngine[P] private extends VaporsEngine[Id, P] {
  override type Result[R] = StandardVaporsResult[R, P]
  override type ExtractParam = DummyImplicit

  override final def eval[R](
    rootExpr: RootExpr[R, P],
    factTable: FactTable,
  ): StandardVaporsResult[R, P] = {
    StandardVaporsResult(InterpretExprAsResultFn(rootExpr)(ExprInput.fromFactTable(factTable)))
  }

  override final def extract[R](wrapped: Id[R])(implicit param: DummyImplicit): R = wrapped
}

/**
  * A simpler, more efficient evaluator that returns a stack-safe cats-effect [[IO]].
  *
  * This only returns the result without any concern for evidence tracking or post-processing.
  */
object CatsEffectSimpleVaporsEngine extends CatsEffectSimpleVaporsEngine
sealed class CatsEffectSimpleVaporsEngine private extends VaporsEngine[IO, Unit] {
  override type Result[R] = CatsEffectSimpleVaporsResult[R]
  override type ExtractParam = IORuntime

  override def eval[R](
    rootExpr: RootExpr[R, Unit],
    factTable: FactTable,
  ): CatsEffectSimpleVaporsResult[R] = {
    CatsEffectSimpleVaporsResult(
      rootExpr.visit(new InterpretExprAsSimpleCatsEffectValue(ExprInput.fromFactTable(factTable))),
    )
  }

  override def extract[R](wrapped: IO[R])(implicit param: IORuntime): R = wrapped.unsafeRunSync()
}
