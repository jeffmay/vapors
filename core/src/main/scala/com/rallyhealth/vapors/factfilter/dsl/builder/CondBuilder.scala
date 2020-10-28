package com.rallyhealth.vapors.factfilter.dsl.builder

import cats.Eq
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra.ExpAlg
import com.rallyhealth.vapors.core.data.{NamedLens, Window}
import com.rallyhealth.vapors.factfilter.dsl
import com.rallyhealth.vapors.factfilter.dsl.{equalTo, CondExp}

object CondBuilder {

  private final val any: CondBuilder[Any, Any] = new CondBuilder(NamedLens.id)

  def of[V]: CondBuilder[V, V] = any.asInstanceOf[CondBuilder[V, V]]

  final class IterableOps[T, C[_], V](private val wrap: CondExp[C[V]] => CondExp[T]) extends AnyVal {

    def exists(buildExp: CondBuilder[V, V] => CondExp[V])(implicit ev: C[V] <:< IterableOnce[V]): CondExp[T] = {
      val cond = buildExp(CondBuilder.of[V])
      wrap(dsl.exists(cond))
    }

    def forall(buildExp: CondBuilder[V, V] => CondExp[V])(implicit ev: C[V] <:< IterableOnce[V]): CondExp[T] = {
      val cond = buildExp(CondBuilder.of[V])
      wrap(dsl.forall(cond))
    }
  }

  implicit def asIterableOps[T, C[x] <: IterableOnce[x], V](builder: CondBuilder[T, C[V]]): IterableOps[T, C, V] = {
    new IterableOps[T, C, V](builder.wrap)
  }

}

final class CondBuilder[T, U](private val lens: NamedLens[T, U]) extends AnyVal {

  private def wrap(cond: CondExp[U]): CondExp[T] =
    FreeApplicative.lift(ExpAlg.Select[T, U, Boolean](lens, cond))

  def at[V](selector: NamedLens[T, U] => NamedLens[T, V]): CondBuilder[T, V] = new CondBuilder(selector(lens))

  def withValueAt[V](
    selector: NamedLens[T, U] => NamedLens[T, V],
  )(
    buildExp: CondBuilder[T, V] => CondExp[T],
  ): CondExp[T] = {
    buildExp(at(selector))
  }

  def in(set: Set[U]): CondExp[T] = wrap(dsl.in(set))

  def within(window: Window[U]): CondExp[T] = wrap(dsl.within(window))

  def >(min: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.greaterThan(min))
  def >=(min: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.greaterThanOrEqual(min))
  def <(max: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.lessThan(max))
  def <=(max: U)(implicit ord: Ordering[U]): CondExp[T] = within(Window.lessThanOrEqual(max))
  def ===(value: U)(implicit ord: Eq[U]): CondExp[T] = wrap(equalTo(value))
}
