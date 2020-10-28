package com.rallyhealth.vapors.factfilter.dsl.builder

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg
import com.rallyhealth.vapors.core.data.NamedLens
import com.rallyhealth.vapors.factfilter.data.{NoFactsMatch, TypedFact, TypedResultSet}
import com.rallyhealth.vapors.factfilter.dsl.{CondExp, Exp}

final class FactFilterBuilder[U]
  extends FilterBuilder[List, TypedFact[U], TypedFact[U], TypedResultSet[U]](
    NamedLens.id[TypedFact[U]],
    _.toList,
    _.iterator.toList,
    TypedResultSet(_),
    _ => NoFactsMatch(),
  ) {

  def value: FilterBuilder[List, TypedFact[U], U, TypedResultSet[U]] = at(_.field("value", _.value))
}

class FilterBuilder[C[_], T, U, A] private[dsl] (
  protected val itemLens: NamedLens[T, U],
  protected val toIterable: C[T] => IterableOnce[T],
  protected val fromIterable: IterableOnce[T] => C[T],
  protected val withMatching: List[T] => A,
  protected val ifNoMatch: List[T] => A,
) {

  def at[V](selector: NamedLens[T, U] => NamedLens[T, V]): FilterBuilder[C, T, V, A] = {
    new FilterBuilder(
      selector(itemLens),
      toIterable,
      fromIterable,
      withMatching,
      ifNoMatch,
    )
  }

  def exists(buildExp: CondBuilder[T, U] => CondExp[T]): Exp[C[T], A] = {
    FreeApplicative.lift {
      ExpAlg.Exists[C[T], T, A](
        toIterable,
        buildExp(new CondBuilder(itemLens)),
        nel => withMatching(nel.toList),
        all => ifNoMatch(toIterable(all).iterator.toList),
      )
    }
  }

  def forall(buildExp: CondBuilder[T, U] => CondExp[T]): Exp[C[T], A] = {
    FreeApplicative.lift {
      ExpAlg.ForAll[C[T], T, A](
        toIterable,
        buildExp(new CondBuilder(itemLens)),
        nel => ifNoMatch(nel.toList),
        all => withMatching(toIterable(all).iterator.toList),
      )
    }
  }
}
