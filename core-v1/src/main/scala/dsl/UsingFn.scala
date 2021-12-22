package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

sealed abstract class UsingFn[-I, Fn[-_, +_], OP[_]](definitions: Seq[Expr.Definition[I, OP]]) {

  protected def supply[NI <: I, O](buildExpr: Fn[NI, O]): Expr[NI, O, OP]

  final def thenReturn[O : OP](buildExpr: Fn[Any, O]): Expr.UsingDefinitions[I, O, OP] =
    Expr.UsingDefinitions[I, O, OP](definitions, supply(buildExpr))

  // This can be removed in Scala 3 with the use of dependent function types
  abstract class UsingFnWithInput[NI] private[UsingFn] {
    def apply[O](buildExpr: Fn[NI, O])(implicit opO: OP[O]): Expr.UsingDefinitions[NI, O, OP]
  }

  final def usingInput[NI <: I]: UsingFnWithInput[NI] =
    new UsingFnWithInput[NI] {
      override def apply[O](buildExpr: Fn[NI, O])(implicit opO: OP[O]): Expr.UsingDefinitions[NI, O, OP] =
        Expr.UsingDefinitions(definitions, supply(buildExpr))
    }
}

final class UsingFn1[I, A, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
) extends UsingFn[I, Lambda[(`-i`, `+o`) => Expr[Any, Seq[A], OP] => Expr[i, o, OP]], OP](definitions) {
  override protected def supply[NI <: I, O](buildExpr: Expr[Any, Seq[A], OP] => Expr[NI, O, OP]): Expr[NI, O, OP] =
    buildExpr(a)
}

final class UsingFn2[I, A, B, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
  b: Expr[Any, Seq[B], OP],
) extends UsingFn[I, Lambda[(`-i`, `+o`) => (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP]) => Expr[i, o, OP]], OP](
    definitions,
  ) {
  override protected def supply[NI <: I, O](
    buildExpr: (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP]) => Expr[NI, O, OP],
  ): Expr[NI, O, OP] =
    buildExpr(a, b)
}

final class UsingFn3[I, A, B, C, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
  b: Expr[Any, Seq[B], OP],
  c: Expr[Any, Seq[C], OP],
) extends UsingFn[I, Lambda[
    (`-i`, `+o`) => (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP], Expr[Any, Seq[C], OP]) => Expr[i, o, OP],
  ], OP](
    definitions,
  ) {
  override protected def supply[NI <: I, O](
    buildExpr: (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP], Expr[Any, Seq[C], OP]) => Expr[NI, O, OP],
  ): Expr[NI, O, OP] =
    buildExpr(a, b, c)
}

final class UsingFn4[I, A, B, C, D, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
  b: Expr[Any, Seq[B], OP],
  c: Expr[Any, Seq[C], OP],
  d: Expr[Any, Seq[D], OP],
) extends UsingFn[I, Lambda[
    (`-i`, `+o`) => (
      Expr[Any, Seq[A], OP],
      Expr[Any, Seq[B], OP],
      Expr[Any, Seq[C], OP],
      Expr[Any, Seq[D], OP],
    ) => Expr[i, o, OP],
  ], OP](
    definitions,
  ) {
  override protected def supply[NI <: I, O](
    buildExpr: (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP], Expr[Any, Seq[C], OP],
      Expr[Any, Seq[D], OP]) => Expr[NI, O, OP],
  ): Expr[NI, O, OP] =
    buildExpr(a, b, c, d)
}

final class UsingFn5[I, A, B, C, D, E, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
  b: Expr[Any, Seq[B], OP],
  c: Expr[Any, Seq[C], OP],
  d: Expr[Any, Seq[D], OP],
  e: Expr[Any, Seq[E], OP],
) extends UsingFn[I, Lambda[
    (`-i`, `+o`) => (
      Expr[Any, Seq[A], OP],
      Expr[Any, Seq[B], OP],
      Expr[Any, Seq[C], OP],
      Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP],
    ) => Expr[i, o, OP],
  ], OP](
    definitions,
  ) {
  override protected def supply[NI <: I, O](
    buildExpr: (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP], Expr[Any, Seq[C], OP], Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP]) => Expr[NI, O, OP],
  ): Expr[NI, O, OP] =
    buildExpr(a, b, c, d, e)
}

final class UsingFn6[I, A, B, C, D, E, F, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
  b: Expr[Any, Seq[B], OP],
  c: Expr[Any, Seq[C], OP],
  d: Expr[Any, Seq[D], OP],
  e: Expr[Any, Seq[E], OP],
  f: Expr[Any, Seq[F], OP],
) extends UsingFn[I, Lambda[
    (`-i`, `+o`) => (
      Expr[Any, Seq[A], OP],
      Expr[Any, Seq[B], OP],
      Expr[Any, Seq[C], OP],
      Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP],
      Expr[Any, Seq[F], OP],
    ) => Expr[i, o, OP],
  ], OP](
    definitions,
  ) {
  override protected def supply[NI <: I, O](
    buildExpr: (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP], Expr[Any, Seq[C], OP], Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP], Expr[Any, Seq[F], OP]) => Expr[NI, O, OP],
  ): Expr[NI, O, OP] =
    buildExpr(a, b, c, d, e, f)
}

final class UsingFn7[I, A, B, C, D, E, F, G, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
  b: Expr[Any, Seq[B], OP],
  c: Expr[Any, Seq[C], OP],
  d: Expr[Any, Seq[D], OP],
  e: Expr[Any, Seq[E], OP],
  f: Expr[Any, Seq[F], OP],
  g: Expr[Any, Seq[G], OP],
) extends UsingFn[I, Lambda[
    (`-i`, `+o`) => (
      Expr[Any, Seq[A], OP],
      Expr[Any, Seq[B], OP],
      Expr[Any, Seq[C], OP],
      Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP],
      Expr[Any, Seq[F], OP],
      Expr[Any, Seq[G], OP],
    ) => Expr[i, o, OP],
  ], OP](
    definitions,
  ) {
  override protected def supply[NI <: I, O](
    buildExpr: (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP], Expr[Any, Seq[C], OP], Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP], Expr[Any, Seq[F], OP], Expr[Any, Seq[G], OP]) => Expr[NI, O, OP],
  ): Expr[NI, O, OP] =
    buildExpr(a, b, c, d, e, f, g)
}

final class UsingFn8[I, A, B, C, D, E, F, G, H, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
  b: Expr[Any, Seq[B], OP],
  c: Expr[Any, Seq[C], OP],
  d: Expr[Any, Seq[D], OP],
  e: Expr[Any, Seq[E], OP],
  f: Expr[Any, Seq[F], OP],
  g: Expr[Any, Seq[G], OP],
  h: Expr[Any, Seq[H], OP],
) extends UsingFn[I, Lambda[
    (`-i`, `+o`) => (
      Expr[Any, Seq[A], OP],
      Expr[Any, Seq[B], OP],
      Expr[Any, Seq[C], OP],
      Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP],
      Expr[Any, Seq[F], OP],
      Expr[Any, Seq[G], OP],
      Expr[Any, Seq[H], OP],
    ) => Expr[i, o, OP],
  ], OP](
    definitions,
  ) {
  override protected def supply[NI <: I, O](
    buildExpr: (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP], Expr[Any, Seq[C], OP], Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP], Expr[Any, Seq[F], OP], Expr[Any, Seq[G], OP], Expr[Any, Seq[H], OP]) => Expr[NI, O, OP],
  ): Expr[NI, O, OP] =
    buildExpr(a, b, c, d, e, f, g, h)
}

final class UsingFn9[I, A, B, C, D, E, F, G, H, J, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
  b: Expr[Any, Seq[B], OP],
  c: Expr[Any, Seq[C], OP],
  d: Expr[Any, Seq[D], OP],
  e: Expr[Any, Seq[E], OP],
  f: Expr[Any, Seq[F], OP],
  g: Expr[Any, Seq[G], OP],
  h: Expr[Any, Seq[H], OP],
  j: Expr[Any, Seq[J], OP],
) extends UsingFn[I, Lambda[
    (`-i`, `+o`) => (
      Expr[Any, Seq[A], OP],
      Expr[Any, Seq[B], OP],
      Expr[Any, Seq[C], OP],
      Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP],
      Expr[Any, Seq[F], OP],
      Expr[Any, Seq[G], OP],
      Expr[Any, Seq[H], OP],
      Expr[Any, Seq[J], OP],
    ) => Expr[i, o, OP],
  ], OP](
    definitions,
  ) {
  override protected def supply[NI <: I, O](
    buildExpr: (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP], Expr[Any, Seq[C], OP], Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP], Expr[Any, Seq[F], OP], Expr[Any, Seq[G], OP], Expr[Any, Seq[H], OP],
      Expr[Any, Seq[J], OP]) => Expr[NI, O, OP],
  ): Expr[NI, O, OP] =
    buildExpr(a, b, c, d, e, f, g, h, j)
}

final class UsingFn10[I, A, B, C, D, E, F, G, H, J, K, OP[_]] private[dsl] (
  definitions: Seq[Expr.Definition[I, OP]],
  a: Expr[Any, Seq[A], OP],
  b: Expr[Any, Seq[B], OP],
  c: Expr[Any, Seq[C], OP],
  d: Expr[Any, Seq[D], OP],
  e: Expr[Any, Seq[E], OP],
  f: Expr[Any, Seq[F], OP],
  g: Expr[Any, Seq[G], OP],
  h: Expr[Any, Seq[H], OP],
  j: Expr[Any, Seq[J], OP],
  k: Expr[Any, Seq[K], OP],
) extends UsingFn[I, Lambda[
    (`-i`, `+o`) => (
      Expr[Any, Seq[A], OP],
      Expr[Any, Seq[B], OP],
      Expr[Any, Seq[C], OP],
      Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP],
      Expr[Any, Seq[F], OP],
      Expr[Any, Seq[G], OP],
      Expr[Any, Seq[H], OP],
      Expr[Any, Seq[J], OP],
      Expr[Any, Seq[K], OP],
    ) => Expr[i, o, OP],
  ], OP](
    definitions,
  ) {
  override protected def supply[NI <: I, O](
    buildExpr: (Expr[Any, Seq[A], OP], Expr[Any, Seq[B], OP], Expr[Any, Seq[C], OP], Expr[Any, Seq[D], OP],
      Expr[Any, Seq[E], OP], Expr[Any, Seq[F], OP], Expr[Any, Seq[G], OP], Expr[Any, Seq[H], OP], Expr[Any, Seq[J], OP],
      Expr[Any, Seq[K], OP]) => Expr[NI, O, OP],
  ): Expr[NI, O, OP] =
    buildExpr(a, b, c, d, e, f, g, h, j, k)
}
