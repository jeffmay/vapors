package com.rallyhealth.vapors.v1
package dsl

import algebra.Expr
import debug.HasShow
import data.Justified

import cats.Show
import cats.Show.ContravariantShow
import zio.prelude.classic.Contravariant

trait WrappedBuildExpr2 {

  type OP[_]

  type W[+_]

  type Wrapped[V] = V match
    case Option[v] => Option[W[v]]
    case Map[k, v] => Map[k, W[v]]
    case Seq[v] => Seq[W[v]]
    case _ => W[V]

  protected def wrapConstInner[V](value: V): W[V]

  protected def wrapConstOuter[V : OP](value: V): Wrapped[V] = value match
    case c: Option[_] => c.map(wrapConstInner)
    case c: Map[_, _] => c.map((k, v) => k -> wrapConstInner(v))
    case c: Seq[_] => c.map(wrapConstInner)
    case _ => wrapConstInner(value)

}

class JustifiedBuildExprDsl2 extends WrappedBuildExpr2 {

  type W[+O] = Justified[O]

//  type ConstInput[I, V] = I match
//    case Option[V] => Option[W[V]]
//    case Map[k, V] => Map[k, W[V]]
//    case Seq[V] => Seq[W[V]]
//    case V => W[V]

//  type Wrapped[I] = I match
//    case Option[v] => Option[Justified[v]]
//    case Map[k, v] => Map[k, Justified[v]]
//    case Seq[v] => Seq[Justified[v]]
//    case _ => Justified[I]

//  private def wrap[I](value: I): Wrapped[I] = value match
//    case x: Option[_] => x.map(wrapConst.wrapConst(_))
//    case x: Map[_, _] => x.map((k, v) => k -> Justified.byConst(v))
//    case x: Seq[_] => x.map(Justified.byConst)
//    case _ => Justified.byConst(value)

  type OP[O] = HasShow[O]

//  protected val wrapConst: WrapConst[W, OP] = Justified.wrapConst

//  protected val wrapParam: WrapParam[W, OP] = HasShow.wrapParam[W]

//  given param[O : OP]: OP[W[O]] = wrapParam.wrapParam[O]

  given wrapConstOP[O](using op: HasShow[O]): HasShow[Wrapped[O]] = new HasShow[Wrapped[O]] {
//    private given showInner: Show[O] = Show.show(op.show.show)
    private def showJustified(value: Any): String = {
      ???
    }
    override val show: ContravariantShow[Wrapped[O]] = new Show[Wrapped[O]] {
      override def show(v: Wrapped[O]): String = v match
        case c: Option[Any] => c.map(showJustified).toString
        case c: Map[_, Any] => c.map((k, v) => (k, showJustified(v))).toString
        case c: Seq[Any] => c.map(showJustified).toString
        case c: Justified[Any] => showJustified(c)
    }
  }

  override protected def wrapConstInner[V](value: V): Justified[V] = Justified.byConst(value)

  extension [O : OP] (value: O) inline def const: Expr.Const[Wrapped[O], OP] =
    Expr.Const(wrapConstOuter(value))
}
