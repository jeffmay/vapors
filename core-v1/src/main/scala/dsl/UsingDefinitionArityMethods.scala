package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import data.FactTypeSet

trait UsingDefinitionArityMethods {
  self: DslTypes =>

  // For some reason, IntelliJ highlights errors in this file unless this method is defined here as well
  protected def valuesOfType[T](
    factTypeSet: FactTypeSet[T],
  )(implicit
    opT: OP[T],
    opTs: OP[Seq[W[T]]],
  ): Expr.ValuesOfType[T, W[T], OP]

  def using[I, A : OP](a: Expr.Define[I, Any, A, OP])(implicit opSA: OP[Seq[W[A]]]): UsingFn1[I, W[A], OP] =
    new UsingFn1(Seq(a), valuesOfType(a.factType))

  def using[I, A : OP, B : OP](
    a: Expr.Define[I, Any, A, OP],
    b: Expr.Define[I, Any, B, OP],
  )(implicit
    opSA: OP[Seq[W[A]]],
    opSB: OP[Seq[W[B]]],
  ): UsingFn2[I, W[A], W[B], OP] =
    new UsingFn2(Seq(a, b), valuesOfType(a.factType), valuesOfType(b.factType))

  def using[I, A : OP, B : OP, C : OP](
    a: Expr.Define[I, Any, A, OP],
    b: Expr.Define[I, Any, B, OP],
    c: Expr.Define[I, Any, C, OP],
  )(implicit
    opSA: OP[Seq[W[A]]],
    opSB: OP[Seq[W[B]]],
    opSC: OP[Seq[W[C]]],
  ): UsingFn3[I, W[A], W[B], W[C], OP] =
    new UsingFn3(Seq(a, b, c), valuesOfType(a.factType), valuesOfType(b.factType), valuesOfType(c.factType))

  def using[I, A : OP, B : OP, C : OP, D : OP](
    a: Expr.Define[I, Any, A, OP],
    b: Expr.Define[I, Any, B, OP],
    c: Expr.Define[I, Any, C, OP],
    d: Expr.Define[I, Any, D, OP],
  )(implicit
    opSA: OP[Seq[W[A]]],
    opSB: OP[Seq[W[B]]],
    opSC: OP[Seq[W[C]]],
    opSD: OP[Seq[W[D]]],
  ): UsingFn4[I, W[A], W[B], W[C], W[D], OP] =
    new UsingFn4(
      Seq(a, b, c, d),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
    )

  def using[I, A : OP, B : OP, C : OP, D : OP, E : OP](
    a: Expr.Define[I, Any, A, OP],
    b: Expr.Define[I, Any, B, OP],
    c: Expr.Define[I, Any, C, OP],
    d: Expr.Define[I, Any, D, OP],
    e: Expr.Define[I, Any, E, OP],
  )(implicit
    opSA: OP[Seq[W[A]]],
    opSB: OP[Seq[W[B]]],
    opSC: OP[Seq[W[C]]],
    opSD: OP[Seq[W[D]]],
    opSE: OP[Seq[W[E]]],
  ): UsingFn5[I, W[A], W[B], W[C], W[D], W[E], OP] =
    new UsingFn5(
      Seq(a, b, c, d, e),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
      valuesOfType(e.factType),
    )

  def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP](
    a: Expr.Define[I, Any, A, OP],
    b: Expr.Define[I, Any, B, OP],
    c: Expr.Define[I, Any, C, OP],
    d: Expr.Define[I, Any, D, OP],
    e: Expr.Define[I, Any, E, OP],
    f: Expr.Define[I, Any, F, OP],
  )(implicit
    opSA: OP[Seq[W[A]]],
    opSB: OP[Seq[W[B]]],
    opSC: OP[Seq[W[C]]],
    opSD: OP[Seq[W[D]]],
    opSE: OP[Seq[W[E]]],
    opSF: OP[Seq[W[F]]],
  ): UsingFn6[I, W[A], W[B], W[C], W[D], W[E], W[F], OP] =
    new UsingFn6(
      Seq(a, b, c, d, e, f),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
      valuesOfType(e.factType),
      valuesOfType(f.factType),
    )

  def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP, G : OP](
    a: Expr.Define[I, Any, A, OP],
    b: Expr.Define[I, Any, B, OP],
    c: Expr.Define[I, Any, C, OP],
    d: Expr.Define[I, Any, D, OP],
    e: Expr.Define[I, Any, E, OP],
    f: Expr.Define[I, Any, F, OP],
    g: Expr.Define[I, Any, G, OP],
  )(implicit
    opSA: OP[Seq[W[A]]],
    opSB: OP[Seq[W[B]]],
    opSC: OP[Seq[W[C]]],
    opSD: OP[Seq[W[D]]],
    opSE: OP[Seq[W[E]]],
    opSF: OP[Seq[W[F]]],
    opSG: OP[Seq[W[G]]],
  ): UsingFn7[I, W[A], W[B], W[C], W[D], W[E], W[F], W[G], OP] =
    new UsingFn7(
      Seq(a, b, c, d, e, f, g),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
      valuesOfType(e.factType),
      valuesOfType(f.factType),
      valuesOfType(g.factType),
    )

  def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP, G : OP, H : OP](
    a: Expr.Define[I, Any, A, OP],
    b: Expr.Define[I, Any, B, OP],
    c: Expr.Define[I, Any, C, OP],
    d: Expr.Define[I, Any, D, OP],
    e: Expr.Define[I, Any, E, OP],
    f: Expr.Define[I, Any, F, OP],
    g: Expr.Define[I, Any, G, OP],
    h: Expr.Define[I, Any, H, OP],
  )(implicit
    opSA: OP[Seq[W[A]]],
    opSB: OP[Seq[W[B]]],
    opSC: OP[Seq[W[C]]],
    opSD: OP[Seq[W[D]]],
    opSE: OP[Seq[W[E]]],
    opSF: OP[Seq[W[F]]],
    opSG: OP[Seq[W[G]]],
    opSH: OP[Seq[W[H]]],
  ): UsingFn8[I, W[A], W[B], W[C], W[D], W[E], W[F], W[G], W[H], OP] =
    new UsingFn8(
      Seq(a, b, c, d, e, f, g, h),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
      valuesOfType(e.factType),
      valuesOfType(f.factType),
      valuesOfType(g.factType),
      valuesOfType(h.factType),
    )

  def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP, G : OP, H : OP, J : OP](
    a: Expr.Define[I, Any, A, OP],
    b: Expr.Define[I, Any, B, OP],
    c: Expr.Define[I, Any, C, OP],
    d: Expr.Define[I, Any, D, OP],
    e: Expr.Define[I, Any, E, OP],
    f: Expr.Define[I, Any, F, OP],
    g: Expr.Define[I, Any, G, OP],
    h: Expr.Define[I, Any, H, OP],
    j: Expr.Define[I, Any, J, OP],
  )(implicit
    opSA: OP[Seq[W[A]]],
    opSB: OP[Seq[W[B]]],
    opSC: OP[Seq[W[C]]],
    opSD: OP[Seq[W[D]]],
    opSE: OP[Seq[W[E]]],
    opSF: OP[Seq[W[F]]],
    opSG: OP[Seq[W[G]]],
    opSH: OP[Seq[W[H]]],
    opSJ: OP[Seq[W[J]]],
  ): UsingFn9[I, W[A], W[B], W[C], W[D], W[E], W[F], W[G], W[H], W[J], OP] =
    new UsingFn9(
      Seq(a, b, c, d, e, f, g, h, j),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
      valuesOfType(e.factType),
      valuesOfType(f.factType),
      valuesOfType(g.factType),
      valuesOfType(h.factType),
      valuesOfType(j.factType),
    )

  def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP, G : OP, H : OP, J : OP, K : OP](
    a: Expr.Define[I, Any, A, OP],
    b: Expr.Define[I, Any, B, OP],
    c: Expr.Define[I, Any, C, OP],
    d: Expr.Define[I, Any, D, OP],
    e: Expr.Define[I, Any, E, OP],
    f: Expr.Define[I, Any, F, OP],
    g: Expr.Define[I, Any, G, OP],
    h: Expr.Define[I, Any, H, OP],
    j: Expr.Define[I, Any, J, OP],
    k: Expr.Define[I, Any, K, OP],
  )(implicit
    opSA: OP[Seq[W[A]]],
    opSB: OP[Seq[W[B]]],
    opSC: OP[Seq[W[C]]],
    opSD: OP[Seq[W[D]]],
    opSE: OP[Seq[W[E]]],
    opSF: OP[Seq[W[F]]],
    opSG: OP[Seq[W[G]]],
    opSH: OP[Seq[W[H]]],
    opSJ: OP[Seq[W[J]]],
    opSK: OP[Seq[W[K]]],
  ): UsingFn10[I, W[A], W[B], W[C], W[D], W[E], W[F], W[G], W[H], W[J], W[K], OP] =
    new UsingFn10(
      Seq(a, b, c, d, e, f, g, h, j, k),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
      valuesOfType(e.factType),
      valuesOfType(f.factType),
      valuesOfType(g.factType),
      valuesOfType(h.factType),
      valuesOfType(j.factType),
      valuesOfType(k.factType),
    )
}
