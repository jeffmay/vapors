package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import shapeless3.deriving.Const

trait UnwrappedUsingDefinitionArityMethods extends UsingDefinitionArityMethods with UnwrappedDslTypes {

  override def using[I, A : OP](a: Expr.Define[I, Const[Any], A, OP])(implicit opSA: OP[Seq[A]]): UsingFn1[I, A, OP] =
    new UsingFn1(Seq(a), valuesOfType(a.factType))

  override def using[I, A : OP, B : OP](
    a: Expr.Define[I, Const[Any], A, OP],
    b: Expr.Define[I, Const[Any], B, OP],
  )(implicit
    opSA: OP[Seq[A]],
    opSB: OP[Seq[B]],
  ): UsingFn2[I, A, B, OP] =
    new UsingFn2(Seq(a, b), valuesOfType(a.factType), valuesOfType(b.factType))

  override def using[I, A : OP, B : OP, C : OP](
    a: Expr.Define[I, Const[Any], A, OP],
    b: Expr.Define[I, Const[Any], B, OP],
    c: Expr.Define[I, Const[Any], C, OP],
  )(implicit
    opSA: OP[Seq[A]],
    opSB: OP[Seq[B]],
    opSC: OP[Seq[C]],
  ): UsingFn3[I, A, B, C, OP] =
    new UsingFn3(Seq(a, b, c), valuesOfType(a.factType), valuesOfType(b.factType), valuesOfType(c.factType))

  override def using[I, A : OP, B : OP, C : OP, D : OP](
    a: Expr.Define[I, Const[Any], A, OP],
    b: Expr.Define[I, Const[Any], B, OP],
    c: Expr.Define[I, Const[Any], C, OP],
    d: Expr.Define[I, Const[Any], D, OP],
  )(implicit
    opSA: OP[Seq[A]],
    opSB: OP[Seq[B]],
    opSC: OP[Seq[C]],
    opSD: OP[Seq[D]],
  ): UsingFn4[I, A, B, C, D, OP] =
    new UsingFn4(
      Seq(a, b, c, d),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
    )

  override def using[I, A : OP, B : OP, C : OP, D : OP, E : OP](
    a: Expr.Define[I, Const[Any], A, OP],
    b: Expr.Define[I, Const[Any], B, OP],
    c: Expr.Define[I, Const[Any], C, OP],
    d: Expr.Define[I, Const[Any], D, OP],
    e: Expr.Define[I, Const[Any], E, OP],
  )(implicit
    opSA: OP[Seq[A]],
    opSB: OP[Seq[B]],
    opSC: OP[Seq[C]],
    opSD: OP[Seq[D]],
    opSE: OP[Seq[E]],
  ): UsingFn5[I, A, B, C, D, E, OP] =
    new UsingFn5(
      Seq(a, b, c, d, e),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
      valuesOfType(e.factType),
    )

  override def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP](
    a: Expr.Define[I, Const[Any], A, OP],
    b: Expr.Define[I, Const[Any], B, OP],
    c: Expr.Define[I, Const[Any], C, OP],
    d: Expr.Define[I, Const[Any], D, OP],
    e: Expr.Define[I, Const[Any], E, OP],
    f: Expr.Define[I, Const[Any], F, OP],
  )(implicit
    opSA: OP[Seq[A]],
    opSB: OP[Seq[B]],
    opSC: OP[Seq[C]],
    opSD: OP[Seq[D]],
    opSE: OP[Seq[E]],
    opSF: OP[Seq[F]],
  ): UsingFn6[I, A, B, C, D, E, F, OP] =
    new UsingFn6(
      Seq(a, b, c, d, e, f),
      valuesOfType(a.factType),
      valuesOfType(b.factType),
      valuesOfType(c.factType),
      valuesOfType(d.factType),
      valuesOfType(e.factType),
      valuesOfType(f.factType),
    )

  override def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP, G : OP](
    a: Expr.Define[I, Const[Any], A, OP],
    b: Expr.Define[I, Const[Any], B, OP],
    c: Expr.Define[I, Const[Any], C, OP],
    d: Expr.Define[I, Const[Any], D, OP],
    e: Expr.Define[I, Const[Any], E, OP],
    f: Expr.Define[I, Const[Any], F, OP],
    g: Expr.Define[I, Const[Any], G, OP],
  )(implicit
    opSA: OP[Seq[A]],
    opSB: OP[Seq[B]],
    opSC: OP[Seq[C]],
    opSD: OP[Seq[D]],
    opSE: OP[Seq[E]],
    opSF: OP[Seq[F]],
    opSG: OP[Seq[G]],
  ): UsingFn7[I, A, B, C, D, E, F, G, OP] =
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

  override def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP, G : OP, H : OP](
    a: Expr.Define[I, Const[Any], A, OP],
    b: Expr.Define[I, Const[Any], B, OP],
    c: Expr.Define[I, Const[Any], C, OP],
    d: Expr.Define[I, Const[Any], D, OP],
    e: Expr.Define[I, Const[Any], E, OP],
    f: Expr.Define[I, Const[Any], F, OP],
    g: Expr.Define[I, Const[Any], G, OP],
    h: Expr.Define[I, Const[Any], H, OP],
  )(implicit
    opSA: OP[Seq[A]],
    opSB: OP[Seq[B]],
    opSC: OP[Seq[C]],
    opSD: OP[Seq[D]],
    opSE: OP[Seq[E]],
    opSF: OP[Seq[F]],
    opSG: OP[Seq[G]],
    opSH: OP[Seq[H]],
  ): UsingFn8[I, A, B, C, D, E, F, G, H, OP] =
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

  override def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP, G : OP, H : OP, J : OP](
    a: Expr.Define[I, Const[Any], A, OP],
    b: Expr.Define[I, Const[Any], B, OP],
    c: Expr.Define[I, Const[Any], C, OP],
    d: Expr.Define[I, Const[Any], D, OP],
    e: Expr.Define[I, Const[Any], E, OP],
    f: Expr.Define[I, Const[Any], F, OP],
    g: Expr.Define[I, Const[Any], G, OP],
    h: Expr.Define[I, Const[Any], H, OP],
    j: Expr.Define[I, Const[Any], J, OP],
  )(implicit
    opSA: OP[Seq[A]],
    opSB: OP[Seq[B]],
    opSC: OP[Seq[C]],
    opSD: OP[Seq[D]],
    opSE: OP[Seq[E]],
    opSF: OP[Seq[F]],
    opSG: OP[Seq[G]],
    opSH: OP[Seq[H]],
    opSJ: OP[Seq[J]],
  ): UsingFn9[I, A, B, C, D, E, F, G, H, J, OP] =
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

  override def using[I, A : OP, B : OP, C : OP, D : OP, E : OP, F : OP, G : OP, H : OP, J : OP, K : OP](
    a: Expr.Define[I, Const[Any], A, OP],
    b: Expr.Define[I, Const[Any], B, OP],
    c: Expr.Define[I, Const[Any], C, OP],
    d: Expr.Define[I, Const[Any], D, OP],
    e: Expr.Define[I, Const[Any], E, OP],
    f: Expr.Define[I, Const[Any], F, OP],
    g: Expr.Define[I, Const[Any], G, OP],
    h: Expr.Define[I, Const[Any], H, OP],
    j: Expr.Define[I, Const[Any], J, OP],
    k: Expr.Define[I, Const[Any], K, OP],
  )(implicit
    opSA: OP[Seq[A]],
    opSB: OP[Seq[B]],
    opSC: OP[Seq[C]],
    opSD: OP[Seq[D]],
    opSE: OP[Seq[E]],
    opSF: OP[Seq[F]],
    opSG: OP[Seq[G]],
    opSH: OP[Seq[H]],
    opSJ: OP[Seq[J]],
    opSK: OP[Seq[K]],
  ): UsingFn10[I, A, B, C, D, E, F, G, H, J, K, OP] =
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
