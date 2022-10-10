package com.rallyhealth.vapors.v2
package engine

import algebra.Expr
import logical.Conjunction

import zio.*

class SimpleUIOVisitor[OP[_]] extends Expr.Visitor[UIO, [i, o] =>> i => o, OP] {
  
  override def visitIdent[I]: I => UIO[I] = ZIO.succeed(_)

  override def visitConst[O : OP](expr: Expr.Const[O, OP]): Any => UIO[O] = _ => ZIO.succeed(expr.out)

  override def visitAnd[I, O : Conjunction : OP](expr: Expr.And[I, O, OP]): I => UIO[O] = { i =>
    val zo1 = expr.one.visit(this).apply(i)
    val zo2 = expr.two.visit(this).apply(i)
    Conjunction[O].conjunction(zo1, zo2)
  }
}