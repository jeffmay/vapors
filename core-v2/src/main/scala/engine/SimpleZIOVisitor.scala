package com.rallyhealth.vapors.v2
package engine

import algebra.ZExpr
import logical.Conjunction

import zio.*

class SimpleZIOVisitor extends ZExpr.ZVisitor[ZIO, [i, o] =>> i => o] {
  
  override def visitIdent[I]: I => ZIO[Any, Nothing, I] = ZIO.succeed(_)

  override def visitConst[O](expr: ZExpr.Const[O]): Any => ZIO[Any, Nothing, O] = _ => ZIO.succeed(expr.out)

  override def visitAnd[R, I, E, O: Conjunction](expr: ZExpr.ZAnd[R, I, E, O]): I => ZIO[R, E, O] = { i =>
    val zo1 = expr.one.visit(this).apply(i)
    val zo2 = expr.two.visit(this).apply(i)
    Conjunction[O].conjunction(zo1, zo2)
  }
}