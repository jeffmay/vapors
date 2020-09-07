package com.rallyhealth.vapors.core.data

import com.rallyhealth.vapors.core.macros.NamedLensMacros

case class NamedLens[A, B](
  path: DataPath,
  get: A => B,
) {

  def compose[C](lens: NamedLens[B, C]): NamedLens[A, C] = {
    copy(
      path = this.path ::: lens.path,
      get = get.andThen(lens.get),
    )
  }

  def field[C](
    name: String,
    getter: B => C,
  ): NamedLens[A, C] = {
    copy(
      path = path.atField(name),
      get = this.get.andThen(getter),
    )
  }

  def atKey[C[x, y] <: Map[x, y], K : ValidDataPathKey, V](
    key: K,
  )(implicit
    ev: B <:< C[K, V],
  ): NamedLens[A, Option[V]] = {
    copy(
      path = path.atKey(key),
      get = get.andThen(ev).andThen(_.get(key)),
    )
  }

  def head[C[x] <: Iterable[x], V](implicit ev: B <:< C[V]): NamedLens[A, Option[V]] = {
    copy(
      path = path.atHead,
      get = get.andThen(ev).andThen(_.headOption),
    )
  }
}

object NamedLens {

  def id[A]: NamedLens[A, A] = NamedLens(DataPath(Nil), identity[A])

  implicit class Selector[A, B](val lens: NamedLens[A, B]) {
    def select[C](getter: B => C): NamedLens[A, C] = macro NamedLensMacros.selectImpl[A, B, C]
  }

}
