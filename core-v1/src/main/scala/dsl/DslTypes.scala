package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

trait DslTypes extends Any {

  type OP[O]

  final type OPW[O] = OP[W[O]]

  type W[+V]

  final type FOP[F[_], O] = F[OP[O]]

  final type ~>[-I, +O] = Expr[I, O, OP]

  final type ~~>[I, +O] = Expr.Identity[I, OP] => Expr[I, O, OP]

  final type AnyExpr = Expr.AnyWith[OP]

  final type Ap[-I, M, +O] = Expr.AndThen[I, M, M, O, OP]

  final type WithinWindowOf[-I, +V] = Expr.WithinWindow[I, V, W, OP]

  final type >=<[-I, +V] = I WithinWindowOf V
}
