package com.rallyhealth.vapors.v2

import algebra.Expr
import engine.SimpleUIOVisitor

import zio.{ZIO, ZIOAppDefault}

object Main extends ZIOAppDefault {
  override def run: ZIO[Any, Any, Any] = {
    import dsl.simple.*
    val x = and(true.const, false.const)
    val res = x.visit(new SimpleUIOVisitor)
    for {
      out <- res(())
      _ <- ZIO.logInfo(s"$x produced $out")
    } yield ()
  }
}
