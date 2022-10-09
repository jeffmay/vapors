package com.rallyhealth.vapors.v2
package algebra

import logical.Conjunction
import optical.Extracting

import zio.prelude.*

// TODO: Remove Z prefix?
type Expr[-I, +E, +O] = ZExpr[Any, I, E, O]
final val Expr = ZExpr

sealed abstract class ZExpr[-R, -I, +E, +O]:
  def visit[Z[-r, +e, +o], F[-i, +o]](v: ZExpr.ZVisitor[Z, F]): F[I, Z[R, E, O]]

object ZExpr:

  // TODO: How to use R parameter instead of OP[_]?
  trait ZVisitor[Z[-r, +e, +a], ~:>[-i, +o]] {
    def visitIdent[I]: I ~:> Z[Any, Nothing, I]
    def visitConst[O](expr: Const[O]): Any ~:> Z[Any, Nothing, O]
    def visitAnd[R, I, E, O : Conjunction](expr: ZAnd[R, I, E, O]): I ~:> Z[R, E, O]
  }

  sealed trait Ident[I] extends ZExpr[Any, I, Nothing, I]:
    override def visit[Z[-r, +e, +o], F[-i, +o]](v: ZVisitor[Z, F]): F[I, Z[Any, Nothing, I]] = v.visitIdent[I]
  object Ident extends Ident[?]:
    def apply[I]: Ident[I] = Ident.asInstanceOf[Ident[I]]

  final case class Const[+O](out: O) extends ZExpr[Any, Any, Nothing, O] {
    override def visit[Z[-r, +e, +o], F[-i, +o]](v: ZVisitor[Z, F]): F[Any, Z[Any, Nothing, O]] = v.visitConst(this)
  }

  final case class ZAnd[-R, -I, +E, +O : Conjunction](
    one: ZExpr[R, I, E, O],
    two: ZExpr[R, I, E, O],
  ) extends ZExpr[R, I, E, O] {
    override def visit[Z[-r, +e, +o], F[-i, +o]](v: ZVisitor[Z, F]): F[I, Z[R, E, O]] = v.visitAnd(this)
  }
