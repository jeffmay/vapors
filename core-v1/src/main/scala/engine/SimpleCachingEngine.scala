package com.rallyhealth.vapors.v1

package engine

import algebra.{EqualComparable, Expr, SizeComparable, WindowComparable}
import data.ExtractValue.AsBoolean
import data._
import debug.DebugArgs
import debug.DebugArgs.Invoker
import dsl.{Sortable, ZipToShortest}
import logic.{Conjunction, Disjunction, Negation}

import cats.arrow.Arrow
import cats.data.NonEmptyVector
import cats.{Applicative, Eval, FlatMap, Foldable, Functor, FunctorFilter, Traverse}
import shapeless.HList

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
    implicit val instance: Applicative[CachedResult]
      with Extract[CachedResult]
      with Foldable[CachedResult]
      with Functor[CachedResult] =
      new Applicative[CachedResult] with Extract[CachedResult] with Foldable[CachedResult] with Functor[CachedResult] {

        override def pure[A](x: A): CachedResult[A] = CachedResult(x, Map.empty)

        override def ap[A, B](ff: CachedResult[A => B])(fa: CachedResult[A]): CachedResult[B] = {
          val value = ff.value(fa.value)
          CachedResult(value, fa.cacheState)
        }

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
    with Expr.Visitor[Lambda[(`-i`, `+o`) => i => CachedResult[o]], OP] {

    import cats.implicits._

    /**
      * A simple type alias for defining the [[Arrow]] typeclass.
      */
    private type =*>[-I, +O] = I => CachedResult[O]

    private implicit val arrowCachedFunction: Arrow[=*>] = new Arrow[=*>] {
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
      factTable: FactTable = this.factTable,
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
      factTable: FactTable = this.factTable,
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

    override def visitConvert[I, O : OP](expr: Expr.Convert[I, O, OP]): I => CachedResult[O] = memoize(expr, _) { i =>
      val o = expr.converter(i)
      debugging(expr).invokeAndReturn(state(i, cached(o)))
    }

    override def visitCustomFunction[I, O : OP](expr: Expr.CustomFunction[I, O, OP]): I => CachedResult[O] =
      memoize(expr, _) { i =>
        val o = expr.function(i)
        debugging(expr).invokeAndReturn(state(i, cached(o)))
      }

    override def visitDefine[I, C[_] : Foldable, T](
      expr: Expr.Define[I, C, T, OP],
    )(implicit
      opF: OP[Seq[TypedFact[T]]],
    ): I => CachedResult[Seq[TypedFact[T]]] = memoize(expr, _) { i =>
      val CachedResult(factValues, updatedCached) = expr.defnExpr.visit(this)(i)
      val facts = factValues.toList.map(expr.factType)
      debugging(expr).invokeAndReturn(state((i, factValues), cached(facts, updatedCached)))
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

    override def visitFilter[C[_] : FunctorFilter, A, B : ExtractValue.AsBoolean : OP](
      expr: Expr.Filter[C, A, B, OP],
    )(implicit
      opO: OP[C[A]],
    ): C[A] => CachedResult[C[A]] = memoize(expr, _) { input =>
      // TODO: Is it possible to use cache between elements in the functor?
      val isMatchingResult = expr.conditionExpr.visit(this).andThen(r => ExtractValue.asBoolean(r.value))
      val o = input.filter(isMatchingResult)
      debugging(expr).invokeAndReturn(state(input, cached(o)))
    }

    override def visitFlatten[C[_] : FlatMap, A](
      expr: Expr.Flatten[C, A, OP],
    )(implicit
      opCA: OP[C[A]],
    ): C[C[A]] => CachedResult[C[A]] = memoize(expr, _) { cca =>
      val ca = cca.flatMap(identity)
      debugging(expr).invokeAndReturn(state(cca, cached(ca)))
    }

    override def visitFoldLeft[I, C[_] : Foldable, A, O : OP](
      expr: Expr.FoldLeft[I, C, A, O, OP],
    ): I => CachedResult[O] = memoize(expr, _) { i =>
      val CachedResult(ca, inputResultState) = expr.inputExpr.visit(this)(i)
      val initB = visitWithUpdatedCache(i, expr.initExpr, inputResultState)
      val finalB = ca.foldLeft(initB) {
        case (CachedResult(b, lastCacheState), a) =>
          visitWithUpdatedCache((b, a), expr.foldExpr, lastCacheState)
      }
      debugging(expr).invokeAndReturn(state((i, ca, initB.value), finalB))
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

    override def visitSequence[C[+_] : Traverse, I, O](
      expr: Expr.Sequence[C, I, O, OP],
    )(implicit
      opCO: OP[C[O]],
    ): I => CachedResult[C[O]] = memoize(expr, _) { i =>
      val co = expr.expressions.traverse { e =>
        e.visit(this)(i)
      }
      debugging(expr).invokeAndReturn(state(i, co))
    }

    override def visitSizeIs[I, N : ExtractValue[*, Int], B : ExtractValue.AsBoolean : OP](
      expr: Expr.SizeIs[I, N, B, OP],
    )(implicit
      compare: SizeComparable[I, N, B],
    ): I => CachedResult[B] = memoize(expr, _) { i =>
      val CachedResult(comparedTo, cacheState) = expr.comparedTo.visit(this)(i)
      val o = compare.sizeCompare(i, expr.comparison, comparedTo)
      debugging(expr).invokeAndReturn(state((i, expr.comparison, comparedTo), cached(o, cacheState)))
    }

    override def visitSorted[C[_], A](
      expr: Expr.Sorted[C, A, OP],
    )(implicit
      sortable: Sortable[C, A],
      opAs: OP[C[A]],
    ): C[A] => CachedResult[C[A]] = memoize(expr, _) { i =>
      val unsorted: C[A] = i
      val sorted = sortable.sort(i)
      debugging(expr).invokeAndReturn(state(unsorted, cached(sorted)))
    }

    override def visitUsingDefinitions[I, O : OP](expr: Expr.UsingDefinitions[I, O, OP]): I => CachedResult[O] =
      memoize(expr, _) { i =>
        val CachedResult(newFacts, updatedCache) =
          expr.definitions.foldLeft(CachedResult(Set.empty[Fact], resultCache)) { (last, next) =>
            val CachedResult(nextFacts, updatedCache) = visitWithUpdatedCache(i, next, last.cacheState)
            CachedResult(last.value ++ nextFacts, updatedCache)
          }
        val updatedFactTable = factTable.addAll(newFacts)
        val o = visitWithUpdatedCache(i, expr.thenExpr, updatedCache, updatedFactTable)
        debugging(expr).invokeAndReturn(state((i, newFacts), o, updatedFactTable))
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

    override def visitWhen[I, B : ExtractValue.AsBoolean, O : OP](expr: Expr.When[I, B, O, OP]): I => CachedResult[O] =
      memoize(expr, _) { i =>
        val (cacheState, firstResult) =
          expr.conditionBranches.zipWithIndex.foldLeft((resultCache, None: Option[(O, Int)])) {
            case ((cacheState, None), (cb, idx)) =>
              val condResult = visitWithUpdatedCache(i, cb.whenExpr, cacheState)
              val condIsMet = ExtractValue.asBoolean(condResult.value)
              if (condIsMet) {
                val branchResult = visitWithUpdatedCache(i, cb.thenExpr, condResult.cacheState)
                (branchResult.cacheState, Some(branchResult.value, idx))
              } else {
                (condResult.cacheState, None)
              }
            case (found @ (_, Some(_)), _) => found
          }
        val (o, idx, finalCacheState) = firstResult
          .map {
            case (o, idx) => (o, idx, cacheState)
          }
          .getOrElse {
            val defaultResult = visitWithUpdatedCache(i, expr.defaultExpr, cacheState)
            (defaultResult.value, expr.conditionBranches.length, defaultResult.cacheState)
          }
        debugging(expr).invokeAndReturn(state((i, idx), cached(o, finalCacheState)))
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

    override def visitZipToShortestHList[I, F[+_], WL <: HList, UL <: HList](
      expr: Expr.ZipToShortestHList[I, F, WL, UL, OP],
    )(implicit
      zip: ZipToShortest.Aux[F, WL, OP, UL],
      opO: OP[F[UL]],
    ): I =*> F[UL] = memoize(expr, _) { i =>
      val o = zip.zipToShortestWith(expr.exprHList, this).apply(i)
      debugging(expr).invokeAndReturn(state(i, o))
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
