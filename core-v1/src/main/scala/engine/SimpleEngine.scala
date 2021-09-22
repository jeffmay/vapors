package com.rallyhealth.vapors.v1

package engine

import algebra.Expr
import data.{FactTable, Window}

import cats.{Foldable, Functor}
import com.rallyhealth.vapors.v1.logic.Negation

/**
  * A vapors [[Expr]] interpreter that just builds a simple function without providing any post-processing.
  *
  * This can be used for better performance when you do not need to inspect or serialize the intermediate
  * results of computation.
  */
object SimpleEngine {

  @inline def apply[OP[_]](factTable: FactTable): Visitor[OP] = new Visitor(factTable)

  class Visitor[OP[_]](protected val factTable: FactTable) extends Expr.Visitor[Lambda[(`-I`, `+O`) => I => O], OP] {

    import cats.implicits._

    override def visitAnd[I](expr: Expr.And[I, OP])(implicit opO: OP[Boolean]): I => Boolean = { i =>
      expr.leftExpr.visit(this)(i) && expr.rightExpr.visit(this)(i)
    }

    override def visitAndThen[II, IO : OP, OI, OO : OP](
      expr: Expr.AndThen[II, IO, OI, OO, OP],
    )(implicit
      evBI: IO <:< OI,
    ): II => OO = { ii =>
      val io = expr.inputExpr.visit(this)(ii)
      val oo = expr.outputExpr.visit(this)(io)
      oo
    }

    override def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      expr: Expr.Combine[I, LI, LO, RI, RO, O, OP],
    )(implicit
      evLOisLI: LO <:< LI,
      evROisRI: RO <:< RI,
    ): I => O = { i =>
      val lo = expr.leftExpr.visit(this)(i)
      val ro = expr.rightExpr.visit(this)(i)
      val o = expr.operation(lo, ro)
      o
    }

    override def visitConst[O : OP](expr: Expr.Const[O, OP]): Any => O = { _ =>
      expr.value
    }

    override def visitCustomFunction[I, O : OP](expr: Expr.CustomFunction[I, O, OP]): I => O = expr.function

    override def visitExists[C[_] : Foldable, A](
      expr: Expr.Exists[C, A, OP],
    )(implicit
      opO: OP[Boolean],
    ): C[A] => Boolean = { ce =>
      val matching = expr.conditionExpr.visit(this)
      ce.exists(matching)
    }

    override def visitForAll[C[_] : Foldable, A](
      expr: Expr.ForAll[C, A, OP],
    )(implicit
      opO: OP[Boolean],
    ): C[A] => Boolean = { ce =>
      val matching = expr.conditionExpr.visit(this)
      ce.forall(matching)
    }

    override def visitIdentity[I : OP](expr: Expr.Identity[I, OP]): I => I = i => i

    override def visitMapEvery[C[_] : Functor, A, B](
      expr: Expr.MapEvery[C, A, B, OP],
    )(implicit
      opO: OP[C[B]],
    ): C[A] => C[B] = { i =>
      val mapFn = expr.mapExpr.visit(this)
      i.map(mapFn)
    }

    override def visitNot[I](expr: Expr.Not[I, OP])(implicit opO: OP[Boolean]): I => Boolean = { i =>
      !expr.innerExpr.visit(this)(i)
    }

    override def visitNot2[I, O : Negation : OP](expr: Expr.Not2[I, O, OP]): I => O = { i =>
      val innerResult = expr.innerExpr.visit(this)(i)
      Negation[O].negation(innerResult)
    }

    override def visitNot3[I](
      expr: Expr.Not3[I, OP],
    )(implicit
      opO: OP[Boolean],
      evB: I <:< Boolean,
    ): I => Boolean = { i =>
      !i
    }

    override def visitNot4[I : Negation : OP](expr: Expr.Not4[I, OP]): I => I = { i =>
      Negation[I].negation(i)
    }

    override def visitOr[I](expr: Expr.Or[I, OP])(implicit opO: OP[Boolean]): I => Boolean = { i =>
      expr.leftExpr.visit(this)(i) || expr.rightExpr.visit(this)(i)
    }

    override def visitValuesOfType[T, O](
      expr: Expr.ValuesOfType[T, O, OP],
    )(implicit
      opTs: OP[Seq[O]],
    ): Any => Seq[O] = { _ =>
      val matchingFacts = factTable.getSortedSeq(expr.factTypeSet)
      matchingFacts.map(expr.transform)
    }

    override def visitWithinWindow[I, O](
      expr: Expr.WithinWindow[I, O, OP],
    )(implicit
      opB: OP[Boolean],
    ): I => Boolean = { i =>
      val window = expr.windowExpr.visit(this)(i)
      val value = expr.valueExpr.visit(this)(i)
      Window.contains(window, value)
    }
  }
}
