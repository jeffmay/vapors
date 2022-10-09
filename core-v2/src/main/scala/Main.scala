package com.rallyhealth.vapors.v2

import algebra.ZExpr
import engine.SimpleZIOVisitor

import zio.{ZIO, ZIOAppDefault}

object Main extends ZIOAppDefault {
  override def run: ZIO[Any, Any, Any] = {
    import ZExpr.*
    val x = ZAnd(Const(true), Const(false))
    val res = x.visit(new SimpleZIOVisitor)
    for {
      out <- res(())
      _ <- ZIO.logInfo(s"$x produced $out")
    } yield ()
  }
}
