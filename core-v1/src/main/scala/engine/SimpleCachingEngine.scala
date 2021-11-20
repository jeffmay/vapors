package com.rallyhealth.vapors.v1

package engine

import algebra.{ConcatToHList, EqualComparable, Expr, NonEmptyExprHList, WindowComparable, ZipToHList}
import data.ExtractValue.AsBoolean
import data.{ExprState, ExtractValue, FactTable, Window}
import debug.DebugArgs
import debug.DebugArgs.Invoker
import logic.{Conjunction, Disjunction, Negation}

import cats.arrow.Arrow
import cats.{Foldable, Functor, FunctorFilter, Semigroupal}
import shapeless.{::, HList}

object SimpleCachingEngine {

  type Key = Any
  type Result = Any

  type ResultCache = Map[Key, Result]

  final case class CachedResult[+V](
    value: V,
    cacheState: ResultCache,
  )

  type =*>[-I, +O] = I => CachedResult[O]

  class Visitor[OP[_]](
    protected val factTable: FactTable,
    protected val resultCache: ResultCache,
  ) extends Expr.Visitor[=*>, OP]
    with CommonEngine[OP] {

    import cats.implicits._

    implicit val arrowCachedFunction: Arrow[=*>] = new Arrow[=*>] {
      override def lift[A, B](f: A => B): A =*> B = f.andThen(CachedResult(_, resultCache))
      override def compose[A, B, C](
        f: B =*> C,
        g: A =*> B,
      ): A =*> C = {
        g.andThen { b =>
          val c = f(b.value)
          CachedResult(c.value, b.cacheState ++ c.cacheState)
        }
      }
      override def first[A, B, C](fa: A =*> B): (A, C) =*> (B, C) = {
        case (a, c) =>
          val b = fa(a)
          CachedResult((b.value, c), resultCache ++ b.cacheState)
      }
    }

    protected def visitWithUpdatedCache[I, O](
      input: I,
      expr: Expr[I, O, OP],
      updated: ResultCache,
    ): CachedResult[O] = {
      val visitor = new Visitor[OP](factTable, updated)
      expr.visit(visitor)(input)
    }

    protected def cached[V](
      value: V,
      cacheState: ResultCache = resultCache,
    ): CachedResult[V] = CachedResult(value, cacheState)

    protected def state[I, O](
      input: I,
      output: CachedResult[O],
    ): ExprState[I, CachedResult[O]] = ExprState(factTable, Some(input), Some(output))

    protected def debugging[E <: Expr.AnyWith[OP]](
      expr: E,
    )(implicit
      debugArgs: DebugArgs[E, OP],
    ): InvokeAndReturn[E, OP, debugArgs.In, debugArgs.Out] =
      new InvokeAndReturn(DebugArgs[OP].of(expr)(debugArgs))

    protected def visitWithUpdatedCache2[I, A, B, R](
      input: I,
      xa: Expr[I, A, OP],
      xb: Expr[I, B, OP],
    )(
      operation: (A, B) => R,
    ): (A, B, CachedResult[R]) = {
      val a = xa.visit(this)(input)
      val b = visitWithUpdatedCache(input, xb, a.cacheState)
      val r = operation(a.value, b.value)
      (a.value, b.value, CachedResult(r, b.cacheState))
    }

    // TODO: Use Hash type bounds on I?
    protected def memoize[I, O](
      expr: Expr[I, O, OP],
      input: I,
    )(
      operation: I => CachedResult[O],
    ): CachedResult[O] = {
      val key = (expr, input)
      val CachedResult(value, cacheState) = resultCache.get(key).map(o => cached(o.asInstanceOf[O])).getOrElse {
        operation(input)
      }
      CachedResult(value, cacheState.updated(key, value))
    }

    override def visitAnd[I, B, F[+_]](
      expr: Expr.And[I, B, F, OP],
    )(implicit
      logic: Conjunction[F, B, OP],
      opO: OP[F[B]],
    ): I => CachedResult[F[B]] =
      memoize(expr, _) { i =>
        val (lo, ro, o) = visitWithUpdatedCache2(i, expr.leftExpr, expr.rightExpr) {
          logic.and(_, _)
        }
        debugging(expr).invokeAndReturn(state((i, lo, ro), o))
      }

    override def visitAndThen[II, IO : OP, OI, OO : OP](
      expr: Expr.AndThen[II, IO, OI, OO, OP],
    )(implicit
      evIOisOI: IO <:< OI,
    ): II => CachedResult[OO] = memoize(expr, _) { ii =>
      val io = expr.inputExpr.visit(this)(ii)
      val oi: OI = io.value
      val oo = visitWithUpdatedCache(oi, expr.outputExpr, io.cacheState)
      debugging(expr).invokeAndReturn(state((ii, io.value), oo))
    }

    override def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      expr: Expr.Combine[I, LI, LO, RI, RO, O, OP],
    )(implicit
      evLOisLI: LO <:< LI,
      evROisRI: RO <:< RI,
    ): I => CachedResult[O] = memoize(expr, _) { i =>
      val (lo, ro, o) = visitWithUpdatedCache2(i, expr.leftExpr, expr.rightExpr) { (lo, ro) =>
        expr.operation(lo, ro)
      }
      debugging(expr).invokeAndReturn(state((i, lo, ro), o))
    }

    override def visitConcatToHList[I, F[+_], WL <: HList : OP, UL <: HList](
      expr: Expr.ConcatToHList[I, F, WL, UL, OP],
    ): I => CachedResult[WL] = memoize(expr, _) { i =>
      val o = expr.exprHList.concatToHListWith(ConcatToHList.proxy(this))(i)
      debugging(expr).invokeAndReturn(state(i, o))
    }

    override def visitConst[O : OP](expr: Expr.Const[O, OP]): Any => CachedResult[O] = { i =>
      // NOTE: This result is not memoized as const is simple to execute
      val o = expr.value
      debugging(expr).invokeAndReturn(state(i, cached(o)))
    }

    override def visitConvert[I, O : OP](expr: Expr.Convert[I, O, OP]): I => CachedResult[O] = memoize(expr, _) { i =>
      val o = expr.converter(i)
      debugging(expr).invokeAndReturn(state(i, cached(o)))
    }

    override def visitCustomFunction[I, O : OP](expr: Expr.CustomFunction[I, O, OP]): I => CachedResult[O] =
      memoize(expr, _) { i =>
        val o = expr.function(i)
        debugging(expr).invokeAndReturn(state(i, cached(o)))
      }

    override def visitExists[C[_] : Foldable, A, B : AsBoolean : OP](
      expr: Expr.Exists[C, A, B, OP],
    ): C[A] => CachedResult[B] = memoize(expr, _) { ca =>
      val (results, o, condCacheState) = visitExistsCommon(expr, ca, resultCache) { (a, condCacheState) =>
        val isMatchingResult = visitWithUpdatedCache(a, expr.conditionExpr, condCacheState)
        (isMatchingResult.value, isMatchingResult.cacheState)
      }
      debugging(expr).invokeAndReturn(state((ca, results), cached(o, condCacheState)))
    }

    override def visitFilter[C[_] : FunctorFilter, A, B : AsBoolean : OP](
      expr: Expr.Filter[C, A, B, OP],
    )(implicit
      opO: OP[C[A]],
    ): C[A] => CachedResult[C[A]] = memoize(expr, _) { input =>
      // TODO: Is it possible to use cache between elements in the functor?
      val isMatchingResult = expr.conditionExpr.visit(this).andThen(r => ExtractValue.asBoolean(r.value))
      val o = input.filter(isMatchingResult)
      debugging(expr).invokeAndReturn(state(input, cached(o)))
    }

    override def visitForAll[C[_] : Foldable, A, B : AsBoolean : OP](
      expr: Expr.ForAll[C, A, B, OP],
    ): C[A] => CachedResult[B] = memoize(expr, _) { ca =>
      // TODO: Fix this to match exists
      val isMatchingResult = expr.conditionExpr.visit(this)
      val (results, o) = visitForAllCommon(expr, ca)(isMatchingResult.andThen(_.value))
      debugging(expr).invokeAndReturn(state((ca, results), cached(o)))
    }

    override def visitIdentity[I : OP](expr: Expr.Identity[I, OP]): I => CachedResult[I] = { i =>
      // NOTE: This result is not memoized as identity is simple to execute
      debugging(expr).invokeAndReturn(state(i, cached(i)))
    }

    override def visitIsEqual[I, V, F[+_]](
      expr: Expr.IsEqual[I, V, F, OP],
    )(implicit
      eq: EqualComparable[F, V],
    ): I => CachedResult[F[Boolean]] = memoize(expr, _) { i =>
      val (left, right, isEqual) = visitWithUpdatedCache2(i, expr.leftExpr, expr.rightExpr) { (left, right) =>
        eq.isEqual(left, right)
      }
      debugging(expr).invokeAndReturn(state((i, left, right), isEqual))
    }

    override def visitMapEvery[C[_] : Functor, A, B](
      expr: Expr.MapEvery[C, A, B, OP],
    )(implicit
      opO: OP[C[B]],
    ): C[A] => CachedResult[C[B]] = memoize(expr, _) { ca =>
      val mapFn = expr.mapExpr.visit(this).andThen(_.value)
      // NOTE: This does not utilize caching between elements.
      // Caching would only help improve performance if the collection contains duplicates,
      // however, the likelihood of duplicates is probably rare enough to make caching a
      // pre-mature optimization that would require obstructing the interface. Most values
      // come from facts and all fact types require some definition of order that is used
      // to deduplicate facts.
      val cb = ca.map(mapFn)
      debugging(expr).invokeAndReturn(state(ca, cached(cb)))
    }

    override def visitNot[I, B, F[+_]](
      expr: Expr.Not[I, B, F, OP],
    )(implicit
      logic: Negation[F, B, OP],
      opB: OP[F[B]],
    ): I => CachedResult[F[B]] = memoize(expr, _) { i =>
      val output = expr.innerExpr.visit(this)(i)
      val negatedOutput = logic.not(output.value)
      debugging(expr).invokeAndReturn(state((i, output.value), cached(negatedOutput, output.cacheState)))
    }

    override def visitOr[I, B, F[+_]](
      expr: Expr.Or[I, B, F, OP],
    )(implicit
      logic: Disjunction[F, B, OP],
      opO: OP[F[B]],
    ): I => CachedResult[F[B]] = memoize(expr, _) { i =>
      val (lo, ro, o) = visitWithUpdatedCache2(i, expr.leftExpr, expr.rightExpr) {
        logic.or(_, _)
      }
      debugging(expr).invokeAndReturn(state((i, lo, ro), o))
    }

    override def visitSelect[I, O : OP](expr: Expr.Select[I, O, OP]): I => CachedResult[O] = { i =>
      // NOTE: This result is not memoized as select is simple to execute
      val o = expr.lens.get(i)
      debugging(expr).invokeAndReturn(state((i, expr.lens), cached(o)))
    }

    override def visitValuesOfType[T, O](
      expr: Expr.ValuesOfType[T, O, OP],
    )(implicit
      opTs: OP[Seq[O]],
    ): Any => CachedResult[Seq[O]] = { i =>
      // NOTE: This result is not memoized as valuesOfType is simple to execute and does not depend on the input
      val matchingFacts = factTable.getSortedSeq(expr.factTypeSet)
      val o = matchingFacts.map(expr.transform)
      debugging(expr).invokeAndReturn(state(i, cached(o)))
    }

    override def visitWithinWindow[I, V, F[+_]](
      expr: Expr.WithinWindow[I, V, F, OP],
    )(implicit
      comparison: WindowComparable[F, OP],
      opV: OP[F[V]],
      opW: OP[F[Window[V]]],
      opB: OP[F[Boolean]],
    ): I => CachedResult[F[Boolean]] = memoize(expr, _) { i =>
      val (value, window, o) = visitWithUpdatedCache2(i, expr.valueExpr, expr.windowExpr) { (value, window) =>
        comparison.withinWindow(value, window)
      }
      debugging(expr).invokeAndReturn(state((i, value, window), o))
    }

    override def visitZipToHList[I, F[+_] : Functor : Semigroupal, WL <: HList, UL <: HList](
      expr: Expr.ZipToHList[I, F, WL, UL, OP],
    )(implicit
      opO: OP[F[UL]],
    ): I => CachedResult[F[UL]] = memoize(expr, _) { i =>
      val o = expr.exprHList.zipToHListWith(ZipToHList.proxy(this))(i)
      debugging(expr).invokeAndReturn(state(i, o))
    }
  }

  def evalMultiple[OP[_], O](
    expressions: Seq[Expr[Any, O, OP]],
    factTable: FactTable = FactTable.empty,
    cache: ResultCache = Map.empty,
  ): IndexedSeq[O] = {
    debugMultiple(expressions, factTable, cache)._1.map(_.value)
  }

  def debugMultiple[OP[_], O](
    expressions: Seq[Expr[Any, O, OP]],
    factTable: FactTable = FactTable.empty,
    cache: ResultCache = Map.empty,
  ): (IndexedSeq[CachedResult[O]], ResultCache) = {
    expressions.foldLeft((Vector.empty[CachedResult[O]], cache)) {
      case ((results, cache), next) =>
        val result = next.visit(new Visitor[OP](factTable, cache))(())
        (results :+ result, result.cacheState)
    }
  }

  // TODO: Support a Map[] version with type-safe keys using the OP?
//  def eval[OP[_]](
//    expressions: Seq[Expr[Nothing, Any, OP]],
//    factTable: FactTable,
//  ): UIO[IndexedSeq[Any]] = {
//    for {
//      cache <- Ref.make(Map(): ResultCache)
//      results <- ZIO.foldLeft(expressions)(Vector.empty[Any]) {
//        case (results, next) =>
//          val visitor = new Visitor[OP](factTable, cache)
//          // TODO: Pass in initial input here
//          next.visit(visitor)(ZIO.never).map { result =>
//            results :+ result
//          }
//      }
//    } yield results
//
//  }

  protected implicit class InvokeAndReturn[E <: Expr.AnyWith[OP], OP[_], DI, DO](
    private val invoker: Invoker[E, OP, DI, DO],
  ) extends AnyVal {

    /**
      * A convenience method for returning the output after debugging.
      */
    def invokeAndReturn(args: ExprState[DI, CachedResult[DO]]): CachedResult[DO] = {
      invoker.invokeDebugger(args.mapOutput(_.value))
      args.output
    }
  }

}
