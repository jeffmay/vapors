package com.rallyhealth.vapors.v2
package algebra

import logical.Conjunction
import optical.Extracting

import zio.prelude.*

sealed abstract class Expr[-I, +O, OP[_]]:
  def visit[F[+a], ~>[-i, +o]](v: Expr.Visitor[F, ~>, OP]): I ~> F[O]

object Expr:

  trait Visitor[F[+a], ~>[-i, +o], OP[_]]:
    def visitIdent[I]: I ~> F[I]
    def visitConst[O : OP](expr: Const[O, OP]): Any ~> F[O]
    def visitAnd[I, O : Conjunction : OP](expr: And[I, O, OP]): I ~> F[O]
  end Visitor

  sealed trait Ident[I, OP[_]] extends Expr[I, I, OP]:
    override def visit[F[+o], ~>[-i, +o]](v: Visitor[F, ~>, OP]): I ~> F[I] = v.visitIdent[I]
  case object Ident extends Ident[?, [_] =>> Any]:
    def apply[I, OP[_]]: Ident[I, OP] = Ident.asInstanceOf[Ident[I, OP]]

  final case class Const[+O : OP, OP[_]](out: O) extends Expr[Any, O, OP]:
    override def visit[F[+o], ~>[-i, +o]](v: Visitor[F, ~>, OP]): Any ~> F[O] = v.visitConst(this)

  final case class And[-I, +O : Conjunction : OP, OP[_]](
    one: Expr[I, O, OP],
    two: Expr[I, O, OP],
  ) extends Expr[I, O, OP]:
    override def visit[F[+o], ~>[-i, +o]](v: Visitor[F, ~>, OP]): I ~> F[O] = v.visitAnd(this)
