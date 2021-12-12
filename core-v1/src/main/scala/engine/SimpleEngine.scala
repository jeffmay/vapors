package com.rallyhealth.vapors.v1

package engine

import algebra.{EqualComparable, Expr, Extract, WindowComparable}
import cats.{Foldable, Functor}
import data.{ExprState, ExtractValue, FactTable, Window}
import debug.DebugArgs
import debug.DebugArgs.Invoker
import logic.{Conjunction, Disjunction, Negation}

/**
  * A vapors [[Expr]] interpreter that just builds a simple function without providing any post-processing.
  *
  * This can be used for better performance when you do not need to inspect or serialize the intermediate
  * results of computation.
  */
object SimpleEngine {

  @inline def apply[OP[_]](factTable: FactTable): Visitor[OP] = new Visitor(factTable)

  class Visitor[OP[_]](protected val factTable: FactTable)
    extends Expr.Visitor[Lambda[(`-I`, `+O`) => I => O], OP]
    with CommonEngine[OP] {

    import cats.implicits._

    protected def state[I, O](
      input: I,
      output: O,
    ): ExprState[I, O] = ExprState(factTable, Some(input), Some(output))

    protected def debugging[E <: Expr.AnyWith[OP]](
      expr: E,
    )(implicit
      debugArgs: DebugArgs[E, OP],
    ): InvokeAndReturn[E, OP, debugArgs.In, debugArgs.Out] =
      new InvokeAndReturn(DebugArgs[OP].of(expr)(debugArgs))

    override def visitAnd[I, B, W[+_]](
      expr: Expr.And[I, B, W, OP],
    )(implicit
      logic: Conjunction[W, B, OP],
      opB: OP[W[B]],
    ): I => W[B] = { i =>
      val exprs = expr.leftExpr +: expr.rightExpressions
      val results = exprs.map(_.visit(this)(i))
      val finalResult = results.reduceLeft { (acc, r) =>
        logic.and(acc, r)
      }
      debugging(expr).invokeAndReturn(state((i, results), finalResult))
    }

    override def visitAndThen[II, IO : OP, OI, OO : OP](
      expr: Expr.AndThen[II, IO, OI, OO, OP],
    )(implicit
      evBI: IO <:< OI,
    ): II => OO = { ii =>
      val io = expr.inputExpr.visit(this)(ii)
      val oo = expr.outputExpr.visit(this)(io)
      debugging(expr).invokeAndReturn(state((ii, io), oo))
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
      debugging(expr).invokeAndReturn(state((i, lo, ro), o))
    }

    override def visitConst[O : OP](expr: Expr.Const[O, OP]): Any => O = { i =>
      val o = expr.value
      debugging(expr).invokeAndReturn(state(i, o))
    }

    override def visitCustomFunction[I, O : OP](expr: Expr.CustomFunction[I, O, OP]): I => O = { i =>
      val o = expr.function(i)
      debugging(expr).invokeAndReturn(state(i, o))
    }

    override def visitExists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
      expr: Expr.Exists[C, A, B, OP],
    ): C[A] => B = { ca =>
      val isMatchingResult = expr.conditionExpr.visit(this)
      val (results, o) = visitExistsCommon(expr, ca)(isMatchingResult)
      debugging(expr).invokeAndReturn(state((ca, results), o))
    }

    override def visitForAll[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
      expr: Expr.ForAll[C, A, B, OP],
    ): C[A] => B = { ca =>
      val isMatchingResult = expr.conditionExpr.visit(this)
      val (results, o) = visitForAllCommon(expr, ca)(isMatchingResult)
      debugging(expr).invokeAndReturn(state((ca, results), o))
    }

    override def visitIdentity[I : OP](expr: Expr.Identity[I, OP]): I => I = { i =>
      debugging(expr).invokeAndReturn(state(i, i))
    }

    override def visitIsEqual[I, V, W[+_]](
      expr: Expr.IsEqual[I, V, W, OP],
    )(implicit
      eq: EqualComparable[W, V, OP],
      opV: OP[W[V]],
      opO: OP[W[Boolean]],
    ): I => W[Boolean] = { i =>
      val left = expr.leftExpr.visit(this)(i)
      val right = expr.leftExpr.visit(this)(i)
      val isEqual = eq.isEqual(left, right)
      debugging(expr).invokeAndReturn(state((i, left, right), isEqual))
    }

    override def visitMapEvery[C[_] : Functor, A, B](
      expr: Expr.MapEvery[C, A, B, OP],
    )(implicit
      opO: OP[C[B]],
    ): C[A] => C[B] = { ca =>
      val mapFn = expr.mapExpr.visit(this)
      val cb = ca.map(mapFn)
      debugging(expr).invokeAndReturn(state(ca, cb))
    }

    override def visitNot[I, B, W[+_]](
      expr: Expr.Not[I, B, W, OP],
    )(implicit
      logic: Negation[W, B, OP],
      opB: OP[W[B]],
    ): I => W[B] = { i =>
      val output = expr.innerExpr.visit(this)(i)
      val negatedOutput = logic.not(output)
      debugging(expr).invokeAndReturn(state((i, output), negatedOutput))
    }

    override def visitOr[I, B, W[+_]](
      expr: Expr.Or[I, B, W, OP],
    )(implicit
      logic: Disjunction[W, B, OP],
      opO: OP[W[B]],
    ): I => W[B] = { i =>
      val exprs = expr.leftExpr +: expr.rightExpressions
      val results = exprs.map(_.visit(this)(i))
      val finalResult = results.reduceLeft { (acc, r) =>
        logic.or(acc, r)
      }
      debugging(expr).invokeAndReturn(state((i, results), finalResult))
    }

    override def visitSelect[I, W[+_] : Extract, A, B, O : OP](expr: Expr.Select[I, W, A, B, O, OP]): I => O = { i =>
      val wa = expr.inputExpr.visit(this)(i)
      val a = Extract[W].extract(wa)
      val b = expr.lens.get(a)
      val o = expr.wrapSelected(wa, b)
      debugging(expr).invokeAndReturn(state((i, wa, expr.lens, b), o))
    }

    override def visitValuesOfType[T, O](
      expr: Expr.ValuesOfType[T, O, OP],
    )(implicit
      opTs: OP[Seq[O]],
    ): Any => Seq[O] = { i =>
      val matchingFacts = factTable.getSortedSeq(expr.factTypeSet)
      val o = matchingFacts.map(expr.transform)
      debugging(expr).invokeAndReturn(state(i, o))
    }

    override def visitWithinWindow[I, V, W[+_]](
      expr: Expr.WithinWindow[I, V, W, OP],
    )(implicit
      comparison: WindowComparable[W, OP],
      opV: OP[W[V]],
      opW: OP[W[Window[V]]],
      opB: OP[W[Boolean]],
    ): I => W[Boolean] = { i =>
      val value = expr.valueExpr.visit(this)(i)
      val window = expr.windowExpr.visit(this)(i)
      val o = comparison.withinWindow(value, window)
      debugging(expr).invokeAndReturn(state((i, value, window), o))
    }
  }

  protected implicit class InvokeAndReturn[E <: Expr.AnyWith[OP], OP[_], DI, DO](
    private val invoker: Invoker[E, OP, DI, DO],
  ) extends AnyVal {

    /**
      * A convenience method for returning the output after debugging.
      */
    def invokeAndReturn(args: ExprState[DI, DO]): DO = {
      invoker.invokeDebugger(args)
      args.output
    }
  }
}
