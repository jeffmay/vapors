package com.rallyhealth.vapors.v1

package engine

import algebra.{EqualComparable, Expr, SizeComparable, WindowComparable}
import data._
import debug.DebugArgs
import debug.DebugArgs.Invoker
import dsl.{ConvertToHList, Sortable, ZipToShortest}
import lens.CollectInto
import logic.{Conjunction, Disjunction, Negation}

import cats.{Applicative, Eval, FlatMap, Foldable, Functor, SemigroupK, Traverse}
import shapeless.{HList, TypeCase, Typeable}

import scala.collection.MapView
import scala.collection.immutable.IntMap

/**
  * A vapors [[Expr]] interpreter that just builds a simple function without providing any post-processing.
  *
  * This can be used for better performance when you do not need to inspect or serialize the intermediate
  * results of computation.
  */
object SimpleEngine {

  @inline def apply[OP[_]](factTable: FactTable): Visitor[OP] = new Visitor(factTable)

  class Visitor[OP[_]](protected val factTable: FactTable)
    extends CommonUncachedEngine[OP]
    with Expr.Visitor[Lambda[(`-I`, `+O`) => I => O], OP] {

    import cats.implicits._

    protected def state[I, O](
      input: I,
      output: O,
      factTable: FactTable = this.factTable,
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
      evIOisOI: IO <:< OI,
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

    override def visitContainsAny[I, W[+_] : Extract, C[_] : Foldable, A, B : OP](
      expr: Expr.ContainsAny[I, W, C, A, B, OP],
    ): I => B = { i =>
      val input = expr.inputExpr.visit(this)(i)
      val wrappedValidSet = expr.validValuesExpr.visit(this)(i)
      // TODO: Implement short-circuiting
      // TODO: Move to CommonEngine
      val W = Extract[W]
      val valid = wrappedValidSet.map(W.extract)
      val found = input
        .foldRight(Eval.now(List.empty[W[A]])) { (wa, acc) =>
          val a = W.extract(wa)
          if (valid(a)) acc.map(wa :: _)
          else acc
        }
        .value
      val o = expr.foundMatchingElements(input, wrappedValidSet, found)
      debugging(expr).invokeAndReturn(state((i, input, wrappedValidSet, found), o))
    }

    override def visitConvert[I, O : OP](expr: Expr.Convert[I, O, OP]): I => O = { i =>
      val o = expr.converter(i)
      debugging(expr).invokeAndReturn(state(i, o))
    }

    override def visitCustomFunction[I, O : OP](expr: Expr.CustomFunction[I, O, OP]): I => O = { i =>
      val o = expr.function(i)
      debugging(expr).invokeAndReturn(state(i, o))
    }

    override def visitDefine[I, C[_] : Foldable, T](
      expr: Expr.Define[I, C, T, OP],
    )(implicit
      opF: OP[Seq[TypedFact[T]]],
    ): I => Seq[TypedFact[T]] = { i =>
      val factValues = expr.defnExpr.visit(this)(i)
      val facts = factValues.toList.map(expr.factType(_))
      debugging(expr).invokeAndReturn(state((i, factValues), facts))
    }

    override def visitExists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
      expr: Expr.Exists[C, A, B, OP],
    ): C[A] => B = { ca =>
      val isMatchingResult = expr.conditionExpr.visit(this)
      val (results, o) = visitExistsCommon(expr, ca)(a => isMatchingResult(a))
      debugging(expr).invokeAndReturn(state((ca, results), o))
    }

    override def visitFilter[C[_], A, B : ExtractValue.AsBoolean, D[_]](
      expr: Expr.Filter[C, A, B, D, OP],
    )(implicit
      filter: CollectInto.Filter[C, A, D],
      opO: OP[D[A]],
    ): C[A] => D[A] = { input =>
      val isMatchingResult = expr.conditionExpr.visit(this).andThen(ExtractValue.asBoolean(_))
      val o = filter.filter(input, isMatchingResult)
      debugging(expr).invokeAndReturn(state(input, o))
    }

    override def visitFlatten[C[_] : FlatMap, A](
      expr: Expr.Flatten[C, A, OP],
    )(implicit
      opCA: OP[C[A]],
    ): C[C[A]] => C[A] = { cca =>
      val ca = cca.flatMap(identity)
      debugging(expr).invokeAndReturn(state(cca, ca))
    }

    override def visitFoldLeft[I, C[_] : Foldable, A, O : OP](expr: Expr.FoldLeft[I, C, A, O, OP]): I => O = { i =>
      val ca = expr.inputExpr.visit(this)(i)
      val initB = expr.initExpr.visit(this)(i)
      val finalB = ca.foldLeft(initB) { (b, a) =>
        expr.foldExpr.visit(this)((b, a))
      }
      debugging(expr).invokeAndReturn(state((i, ca, initB), finalB))
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
      val right = expr.rightExpr.visit(this)(i)
      val isEqual = eq.isEqual(left, right)
      debugging(expr).invokeAndReturn(state((i, left, right), isEqual))
    }

    override def visitGetOrElse[I, O : OP](expr: Expr.GetOrElse[I, O, OP]): I => O = { i =>
      val opt = expr.optionExpr.visit(this)(i)
      val o = opt.getOrElse {
        expr.defaultExpr.visit(this)(i)
      }
      debugging(expr).invokeAndReturn(state((i, opt), o))
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

    override def visitMatch[I, S, B : ExtractValue.AsBoolean, O](
      expr: Expr.Match[I, S, B, O, OP],
    )(implicit
      ev: S <:< I,
      opO: OP[Option[O]],
    ): I => Option[O] = { i =>
      val maybeMatch = expr.branches.zipWithIndex.foldLeft(None: Option[(O, Int)]) {
        case (matched @ Some(_), _) => matched
        case (_, (branch: Expr.MatchCase[I, s, B, O, OP], idx)) =>
          branch.cast(i).flatMap { s =>
            val passesGuard = branch.maybeGuardExpr.forall { g =>
              ExtractValue.asBoolean(g.visit(this)(s))
            }
            Option.when(passesGuard) {
              (branch.thenExpr.visit(this)(s), idx)
            }
          }
      }
      val (o, maybeIdx) = maybeMatch match {
        case Some((o, idx)) => (Some(o), Some(idx))
        case _ => (None, None)
      }
      debugging(expr).invokeAndReturn(state((i, maybeIdx), o))
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

    override def visitRegexMatches[I, S, O : OP](expr: Expr.RegexMatches[I, S, O, OP]): I => O = { i =>
      val s = expr.inputExpr.visit(this)(i)
      val str = expr.asString(s)
      val allMatches = expr.regex
        .findAllMatchIn(str)
        .map(RegexMatch.from)
        .to(LazyList)
      val o = expr.asOutput(s, allMatches)
      debugging(expr).invokeAndReturn(state((i, s, expr.regex, allMatches), o))
    }

    override def visitRepeat[I, O](
      expr: Expr.Repeat[I, O, OP],
    )(implicit
      opO: OP[IterableOnce[O]],
    ): I => IterableOnce[O] = { i =>
      val always = Eval.always {
        expr.inputExpr.visit(this)(i)
      }
      val eval = if (expr.recompute) always else always.memoize
      val unlimited = Iterator.continually(eval.value)
      val iterable = expr.limit.fold(unlimited)(unlimited.take)
      debugging(expr).invokeAndReturn(state((i, eval, expr.limit), iterable))
    }

    override def visitSelect[I, A, B, O : OP](expr: Expr.Select[I, A, B, O, OP]): I => O = { i =>
      val a = expr.inputExpr.visit(this)(i)
      val b = expr.lens.get(a)
      val o = expr.wrapSelected(a, b)
      debugging(expr).invokeAndReturn(state((i, a, expr.lens, b), o))
    }

    override def visitSequence[C[+_] : Applicative : SemigroupK : Traverse, I, O](
      expr: Expr.Sequence[C, I, O, OP],
    )(implicit
      opCO: OP[C[O]],
    ): I => C[O] = { i =>
      val co = Traverse[C].map(expr.expressions) { e =>
        e.visit(this)(i)
      }
      debugging(expr).invokeAndReturn(state(i, co))
    }

    override def visitSizeIs[I, N : ExtractValue[*, Int], B : ExtractValue.AsBoolean : OP](
      expr: Expr.SizeIs[I, N, B, OP],
    )(implicit
      compare: SizeComparable[I, N, B],
    ): I => B = { i =>
      val comparedTo = expr.comparedTo.visit(this)(i)
      val o = compare.sizeCompare(i, expr.comparison, comparedTo)
      debugging(expr).invokeAndReturn(state((i, expr.comparison, comparedTo), o))
    }

    override def visitSlice[C[_] : Traverse, A, D[_]](
      expr: Expr.Slice[C, A, D, OP],
    )(implicit
      filter: CollectInto.Filter[C, A, D],
      opO: OP[D[A]],
    ): C[A] => D[A] = { ca =>
      val absRange = expr.range.toAbsolute(ca.size.toInt)
      val slice = filter.collectSomeWithIndex(ca, (a, idx) => Option.when(absRange contains idx)(a))
      debugging(expr).invokeAndReturn(state((ca, absRange), slice))
    }

    override def visitSorted[C[_], A](
      expr: Expr.Sorted[C, A, OP],
    )(implicit
      sortable: Sortable[C, A],
      opAs: OP[C[A]],
    ): C[A] => C[A] = { i =>
      val o = sortable.sort(i)
      debugging(expr).invokeAndReturn(state(i, o))
    }

    override def visitToHList[I, L <: HList : OP](
      expr: Expr.ToHList[I, L, OP],
    )(implicit
      toHL: ConvertToHList[L],
    ): I => L = { i =>
      val fn = toHL.convertToHListWith(expr.exprHList, this)
      debugging(expr).invokeAndReturn(state(i, fn(i)))
    }

    override def visitUsingDefinitions[I, O : OP](expr: Expr.UsingDefinitions[I, O, OP]): I => O = { i =>
      val factSet = expr.definitions.flatMap { defn =>
        defn.visit(this)(i)
      }.toSet
      val updatedFactTable = factTable.addAll(factSet)
      val o = expr.thenExpr.visit(new Visitor[OP](updatedFactTable))(i)
      debugging(expr).invokeAndReturn(state((i, factSet), o, updatedFactTable))
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

    override def visitWhen[I, B : ExtractValue.AsBoolean, O : OP](expr: Expr.When[I, B, O, OP]): I => O = { i =>
      val firstResult = expr.conditionBranches.zipWithIndex.collectFirstSome {
        case (cb, idx) =>
          val condResult = cb.whenExpr.visit(this)(i)
          val condIsMet = ExtractValue.asBoolean(condResult)
          Option.when(condIsMet) {
            (cb.thenExpr.visit(this)(i), idx)
          }
      }
      val (o, idx) = firstResult.getOrElse {
        (expr.defaultExpr.visit(this)(i), expr.conditionBranches.length)
      }
      debugging(expr).invokeAndReturn(state((i, idx), o))
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

    override def visitZipToShortestHList[I, F[+_], WL <: HList, UL <: HList](
      expr: Expr.ZipToShortestHList[I, F, WL, UL, OP],
    )(implicit
      zip: ZipToShortest.Aux[F, WL, OP, UL],
      opO: OP[F[UL]],
    ): I => F[UL] = { i =>
      val o = zip.zipToShortestWith(expr.exprHList, this).apply(i)
      debugging(expr).invokeAndReturn(state(i, o))
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
