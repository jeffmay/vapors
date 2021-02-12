package com.rallyhealth.vapors.core.lens

import cats.data.NonEmptySet
import cats.kernel.Semigroup
import shapeless.{HList, Nat}
import shapeless.ops.hlist.{At, Tupler}
import shapeless.ops.nat.ToInt

import scala.collection.Factory

object NamedLens {

  type Id[A] = NamedLens[A, A]
  val Id: NamedLens[Any, Any] = NamedLens(DataPath(Nil), identity[Any])
  def id[A]: NamedLens[A, A] = Id.asInstanceOf[Id[A]]

  type Fn[A, B] = NamedLens.Id[A] => NamedLens[A, B]

  implicit final class Selector[A, B](val lens: NamedLens[A, B]) extends AnyVal {
    // TODO: Rename downField?
    def select[C](getter: B => C): NamedLens[A, C] = macro NamedLensMacros.selectImpl[A, B, C]
  }

  implicit final class AsIterableBuilder[A, B[x] <: IterableOnce[x], V](private val lens: NamedLens[A, B[V]])
    extends AnyVal {

    def to[C](factory: Factory[V, C]): NamedLens[A, C] =
      lens.copy(get = lens.get.andThen(_.iterator.to(factory)))
  }

  implicit final class AsHListBuilder[A, L <: HList](private val lens: NamedLens[A, L]) extends AnyVal {

    def tupled[T](implicit tupler: Tupler.Aux[L, T]): NamedLens[A, T] =
      lens.copy(
        path = lens.path,
        get = lens.get.andThen(tupler.apply),
      )

//    def at(
//      n: Nat,
//    )(implicit
//      at: At[L, n.N],
//      toInt: ToInt[n.N],
//    ): NamedLens[A, at.Out] =
//      lens.copy(
//        path = lens.path.atKey(Nat.toInt[n.N]),
//        get = lens.get.andThen(at.apply(_)),
//      )

    def at[N <: Nat : ToInt, B](
      n: N,
    )(implicit
      at: At.Aux[L, N, B],
    ): NamedLens[A, B] =
      lens.copy(
        path = lens.path.atKey(Nat.toInt[N]),
        get = lens.get.andThen(at.apply(_)),
      )
  }

}

final case class NamedLens[A, B](
  path: DataPath,
  get: A => B,
) { outer =>

  def andThen[C](lens: NamedLens[B, C]): NamedLens[A, C] = {
    copy(
      path = this.path ::: lens.path,
      get = get.andThen(lens.get),
    )
  }

  // Helpful for building a tuple with the lens itself. Identical to identity, but you don't have to specify the type.
  def self: NamedLens[A, B] = this

  def field[C](
    name: String,
    getter: B => C,
  ): NamedLens[A, C] = {
    copy(
      path = path.atField(name),
      get = this.get.andThen(getter),
    )
  }

  def atKey[K : ValidDataPathKey, V](
    key: K,
  )(implicit
    CI: Indexed[B, K, Option[V]],
  ): NamedLens[A, Option[V]] = {
    copy(
      path = path.atKey(key),
      get = get.andThen(b => CI.get(b)(key)),
    )
  }

  def filterKeys[K : ValidDataPathKey, V : Semigroup](
    keys: NonEmptySet[K],
  )(implicit
    CI: Indexed[B, K, V],
  ): NamedLens[A, V] = {
    import com.rallyhealth.vapors.core.syntax.indexed._
    copy(
      path = path.filterKeys(keys),
      get = get.andThen(_.filterKeys(keys)),
    )
  }

  def headOption[C[x] <: Iterable[x], V](implicit ev: B <:< C[V]): NamedLens[A, Option[V]] = {
    copy(
      path = path.atHead,
      get = get.andThen(b => ev(b).headOption),
    )
  }

  def asHList[L <: HList](implicit ev: B <:< L): NamedLens[A, L] = copy(get = get.andThen(ev))

  def asIterable[V](implicit ev: B <:< IterableOnce[V]): NamedLens.AsIterableBuilder[A, IterableOnce, V] =
    new NamedLens.AsIterableBuilder(this.copy(get = this.get.andThen(ev)))

}
