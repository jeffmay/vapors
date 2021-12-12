package com.rallyhealth.vapors.v1

package dsl

import cats.Functor
import cats.implicits._

/**
  * Extend this trait to provide the template for the standard implicit definitions (with appropriate priority).
  *
  * Every subclass must extend a separate subclass of [[LowPriorityWrapImplicits]] and can just implement each
  * abstract implicit def by invoking the `defn` method of the same name.
  */
trait WrapImplicits {
  self: DslTypes with LowPriorityWrapImplicits =>

  implicit def constFunctor[C[_] : Functor, O : OP](
    implicit
    aot: ConstOutputType[W, O],
  ): ConstOutputType.Aux[W, C[O], C[aot.Out]]

  implicit def selectFunctor[C[_] : Functor, I : OP, O : OP](
    implicit
    aot: SelectOutputType[W, I, O],
  ): SelectOutputType.Aux[W, I, C[O], C[aot.Out]]

}

trait LowPriorityWrapImplicits {
  self: DslTypes =>

  implicit def constId[O : OP]: ConstOutputType.Aux[W, O, W[O]]

  implicit def selectId[I : OP, O : OP]: SelectOutputType.Aux[W, I, O, W[O]]

  /**
    * Use this to implement all implicit `def`s defined by [[WrapImplicits]]
    */
  protected def defn: WrapDefinitions[W, OP]
}

final class WrapDefinitions[W[+_] : WrapConst, OP[_]](implicit wrapElementW: WrapSelected[W, OP]) {

  private val wrapConstW: WrapConst[W] = implicitly

  def constFunctor[C[_] : Functor, O : OP](aot: ConstOutputType[W, O]): ConstOutputType.Aux[W, C[O], C[aot.Out]] =
    new ConstOutputType[W, C[O]] {
      override type Out = C[aot.Out]
      override def wrapConst(value: C[O]): C[aot.Out] = {
        value.map { a =>
          aot.wrapConst(a)
        }
      }
    }

  def selectFunctor[C[_] : Functor, I : OP, O : OP](
    aot: SelectOutputType[W, I, O],
  ): SelectOutputType.Aux[W, I, C[O], C[aot.Out]] =
    new SelectOutputType[W, I, C[O]] {
      override type Out = C[aot.Out]
      override def wrapSelected(
        wrapped: W[I],
        value: C[O],
      ): C[aot.Out] = {
        value.map { a =>
          aot.wrapSelected(wrapped, a)
        }
      }
    }

  def constId[O : OP]: ConstOutputType.Aux[W, O, W[O]] =
    new ConstOutputType[W, O] {
      override type Out = W[O]
      override def wrapConst(value: O): W[O] = wrapConstW.wrapConst(value)
    }

  def selectId[I : OP, O : OP]: SelectOutputType.Aux[W, I, O, W[O]] =
    new SelectOutputType[W, I, O] {
      override type Out = W[O]
      override def wrapSelected(
        wrapped: W[I],
        value: O,
      ): W[O] = wrapElementW.wrapSelected(wrapped, value)
    }
}
