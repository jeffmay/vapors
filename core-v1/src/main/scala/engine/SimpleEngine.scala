package com.rallyhealth.vapors.v1

package engine

import algebra.{CompareWrapped, Expr, WindowComparable}
import data.{ExprState, FactTable, Window}
import logic.Negation

import cats.{Foldable, Functor}

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

    override def visitExists[C[_] : Foldable, A, B : OP](expr: Expr.Exists[C, A, B, OP]): C[A] => B = { ca =>
      val isMatchingResult = expr.conditionExpr.visit(this)
      // Using a foldRight to short-circuit for the simple engine:
//      val (result, foundMatch) = ce.foldRight(Eval.now(expr.emptyOutput, false))) {
//        case ((e, foundMatch), remaining) =>
//          if (foundMatch) Eval.now(e)
//          else
//            remaining.map { case (r, prevMatch) =>
//              val isMatch = prevMatch || matching(e)
//              (expr.foldToResult(r, e, isMatch), isMatch)
//            }
//      }.value

      // Naive implementation will handle justification use cases better
      val matching = ca.foldLeft(List.empty[B]) { (bs, a) =>
        val b = isMatchingResult(a)
        val isMatch = expr.asBoolean(b)
        if (isMatch) b :: bs
        else bs
      }
      val output = expr.combine(matching)
      expr.debugging.attach(ExprState(factTable, Some(ca), Some(output)))
      output
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

    override def visitNot[I, O : Negation : OP](expr: Expr.Not[I, O, OP]): I => O = { i =>
      val innerResult = expr.innerExpr.visit(this)(i)
      Negation[O].negation(innerResult)
    }

    override def visitOr[I](expr: Expr.Or[I, OP])(implicit opO: OP[Boolean]): I => Boolean = { i =>
      expr.leftExpr.visit(this)(i) || expr.rightExpr.visit(this)(i)
    }

    override def visitSelect[I, O : OP](expr: Expr.Select[I, O, OP]): I => O = { i =>
      val output = expr.lens.get(i)
      expr.debugging.attach(ExprState(factTable, Some(i), Some(output)))
      output
    }

    override def visitValuesOfType[T, O](
      expr: Expr.ValuesOfType[T, O, OP],
    )(implicit
      opTs: OP[Seq[O]],
    ): Any => Seq[O] = { _ =>
      val matchingFacts = factTable.getSortedSeq(expr.factTypeSet)
      matchingFacts.map(expr.transform)
    }

    override def visitWithinWindow[I, V : OP, F[+_]](
      expr: Expr.WithinWindow[I, V, F, OP],
    )(implicit
      comparison: WindowComparable[F, OP],
      opB: OP[F[Boolean]],
    ): I => F[Boolean] = { i =>
      val window = expr.windowExpr.visit(this)(i)
      val value = expr.valueExpr.visit(this)(i)
      val output = comparison.withinWindow(value, window)
      expr.debugging.attach(ExprState(factTable, Some((i, value, window)), Some(output)))
      output
    }
  }
}
