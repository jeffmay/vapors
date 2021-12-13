package com.rallyhealth.vapors.v1

package engine

import algebra.{EqualComparable, Expr, WindowComparable}
import cats.data.NonEmptyVector
import data.ExtractValue.AsBoolean
import data.{ExprState, Extract, FactTable, Window}
import debug.DebugArgs
import debug.DebugArgs.Invoker
import logic.{Conjunction, Disjunction, Negation}
import cats.{Eval, Foldable, Functor}

object SimpleCachingEngine {

  // TODO: Use a more type-safe cache with opaque typed keys (rather than Any)

  type Key = Any
  type Result = Any

  type ResultCache = Map[Key, Result]

  final case class CachedResult[+V](
    value: V,
    cacheState: ResultCache,
  )

  final object CachedResult {
    implicit val instance: Extract[CachedResult] with Foldable[CachedResult] with Functor[CachedResult] =
      new Extract[CachedResult] with Foldable[CachedResult] with Functor[CachedResult] {

        override final def extract[A](fa: CachedResult[A]): A = fa.value

        override final def foldLeft[A, B](
          fa: CachedResult[A],
          b: B,
        )(
          f: (B, A) => B,
        ): B = f(b, fa.value)

        override final def foldRight[A, B](
          fa: CachedResult[A],
          lb: Eval[B],
        )(
          f: (A, Eval[B]) => Eval[B],
        ): Eval[B] = f(fa.value, lb)

        override final def map[A, B](fa: CachedResult[A])(f: A => B): CachedResult[B] = {
          val b = f(fa.value)
          CachedResult(b, fa.cacheState)
        }
      }
  }

  class Visitor[OP[_]](
    protected val factTable: FactTable,
    protected val resultCache: ResultCache,
  ) extends CommonEngine[CachedResult, OP]
    with Expr.Visitor[Lambda[(`-I`, `+O`) => I => CachedResult[O]], OP] {

    import cats.implicits._

    protected def visitWithUpdatedCache[I, O](
      input: I,
      expr: Expr[I, O, OP],
      updated: ResultCache,
    ): CachedResult[O] = {
      val visitor = new Visitor[OP](factTable, updated)
      expr.visit(visitor)(input)
    }

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

    protected def visitWithUpdatedCacheN[I, A](
      input: I,
      xs: NonEmptyVector[Expr[I, A, OP]],
    ): CachedResult[NonEmptyVector[A]] = {
      val (cacheState, values) = xs.toVector.foldLeft((resultCache, Vector.empty[A])) {
        case ((updated, values), x) =>
          val result = visitWithUpdatedCache(input, x, updated)
          (result.cacheState, values :+ result.value)
      }
      // Safe: because the original vector is non-empty and the fold always produces a vector of the same size
      CachedResult(NonEmptyVector.fromVectorUnsafe(values), cacheState)
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
        val inputCachedResult = visitWithUpdatedCacheN(i, expr.leftExpr +: expr.rightExpressions)
        val inputs = inputCachedResult.value
        val output = inputs.reduceLeft { (l, r) =>
          logic.and(l, r)
        }
        val result = CachedResult(output, inputCachedResult.cacheState)
        debugging(expr).invokeAndReturn(state((i, inputs), result))
      }

    override def visitAndThen[II, IO : OP, OI, OO : OP](
      expr: Expr.AndThen[II, IO, OI, OO, OP],
    )(implicit
      evIOisOI: IO <:< OI,
    ): II => CachedResult[OO] = memoize(expr, _) { ii =>
      val io = expr.inputExpr.visit(this)(ii)
      val oo = visitWithUpdatedCache(evIOisOI(io.value), expr.outputExpr, io.cacheState)
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

    override def visitConst[O : OP](expr: Expr.Const[O, OP]): Any => CachedResult[O] = { i =>
      // NOTE: This result is not memoized as const is simple to execute
      val o = expr.value
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
      val finalState = visitExistsCommon(expr, CachedResult(ca, resultCache)) { sa =>
        visitWithUpdatedCache(sa.value, expr.conditionExpr, sa.cacheState)
      }
      val (results, o) = finalState.value
      debugging(expr).invokeAndReturn(state((ca, results), cached(o, finalState.cacheState)))
    }

    override def visitForAll[C[_] : Foldable, A, B : AsBoolean : OP](
      expr: Expr.ForAll[C, A, B, OP],
    ): C[A] => CachedResult[B] = memoize(expr, _) { ca =>
      val finalState = visitForAllCommon(expr, CachedResult(ca, resultCache)) { sa =>
        visitWithUpdatedCache(sa.value, expr.conditionExpr, sa.cacheState)
      }
      val (results, o) = finalState.value
      debugging(expr).invokeAndReturn(state((ca, results), cached(o, finalState.cacheState)))
    }

    override def visitIdentity[I : OP](expr: Expr.Identity[I, OP]): I => CachedResult[I] = { i =>
      // NOTE: This result is not memoized as identity is simple to execute
      debugging(expr).invokeAndReturn(state(i, cached(i)))
    }

    override def visitIsEqual[I, V, W[+_]](
      expr: Expr.IsEqual[I, V, W, OP],
    )(implicit
      eq: EqualComparable[W, V, OP],
      opV: OP[W[V]],
      opO: OP[W[Boolean]],
    ): I => CachedResult[W[Boolean]] = memoize(expr, _) { i =>
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
      val inputCachedResult = visitWithUpdatedCacheN(i, expr.leftExpr +: expr.rightExpressions)
      val inputs = inputCachedResult.value
      val output = inputs.reduceLeft { (l, r) =>
        logic.or(l, r)
      }
      val result = CachedResult(output, inputCachedResult.cacheState)
      debugging(expr).invokeAndReturn(state((i, inputs), result))
    }

    override def visitSelect[I, A, B, O : OP](expr: Expr.Select[I, A, B, O, OP]): I => CachedResult[O] =
      memoize(expr, _) { i =>
        val inputResult = expr.inputExpr.visit(this)(i)
        val a = inputResult.value
        val b = expr.lens.get(a)
        val o = expr.wrapSelected(a, b)
        val outputResult = CachedResult(o, inputResult.cacheState)
        debugging(expr).invokeAndReturn(state((i, a, expr.lens, b), outputResult))
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
  }

  // TODO: Support an eval method that returns an HMap with type-safe keys and / or HList?

  def evalMultiple[I, O, OP[_]](
    expressions: Seq[Expr[I, O, OP]],
    factTable: FactTable = FactTable.empty,
    input: I = (),
    cache: ResultCache = Map.empty,
  ): IndexedSeq[O] = {
    debugMultiple(expressions, factTable, input, cache).map(_.value)
  }

  def debugMultiple[I, O, OP[_]](
    expressions: Seq[Expr[I, O, OP]],
    factTable: FactTable = FactTable.empty,
    input: I = (),
    cache: ResultCache = Map.empty,
  ): IndexedSeq[CachedResult[O]] = {
    // Using a mutable var to improve performance by avoiding wrapping and unwrapping tuples
    var lastResultCache = cache
    var results = Vector.empty[CachedResult[O]]
    for (next <- expressions) {
      val result = next.visit(new Visitor[OP](factTable, lastResultCache))(input)
      lastResultCache = result.cacheState
      results :+= result
    }
    results
  }

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
