package com.rallyhealth.vapors.v1

package dsl

import cats.implicits._
import cats.Traverse
import lens.DataPath
import shapeless.<:!<

/**
  * Extend this trait to provide the template for the standard implicit definitions (with appropriate priority).
  *
  * Every subclass must extend a separate subclass of [[LowPriorityWrapImplicits]] and can just implement each
  * abstract implicit def by invoking the `defn` method of the same name.
  */
trait WrapImplicits extends MidPriorityWrapImplicits {
  self: DslTypes =>

  implicit def constTraverse[C[_] : Traverse, O](
    implicit
    sot: SelectOutputType[W, C[O], O],
    opCO: OP[C[O]],
  ): ConstOutputType.Aux[W, C[O], C[sot.Out]]

  implicit def selectOption[I : OP, O : OP](
    implicit
    sot: SelectOutputType[W, I, O],
  ): SelectOutputType.Aux[W, I, Option[O], Option[sot.Out]]

}

trait MidPriorityWrapImplicits extends LowPriorityWrapImplicits {
  self: DslTypes =>

  /**
    * Recursively select into a traversable type to wrap the leaf node type. This makes it easier to determine
    * the next operation you would want to perform on wrapped collection values.
    *
    * @param nt Proof that this collection is not a Tuple. This is required because cats implements Traverse
    *           for the last element of Tuples, which will cause the wrapper to only wrap the last element of
    *           the [[Product]] type. We want to always wrap the whole product value and use a lens to select
    *           a specific element of the product type.
    */
  implicit def selectTraverse[C[_] : Traverse, I : OP, O : OP](
    implicit
    sot: SelectOutputType[W, I, O],
    nt: C[O] <:!< Product,
  ): SelectOutputType.Aux[W, I, C[O], C[sot.Out]]
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

final class WrapDefinitions[W[+_], OP[_]](
  implicit
  wrapElementW: WrapSelected[W, OP],
  wrapConstW: WrapConst[W, OP],
) {

  def constTraverse[C[_] : Traverse, O](
    sot: SelectOutputType[W, C[O], O],
  )(implicit
    opCO: OP[C[O]],
  ): ConstOutputType.Aux[W, C[O], C[sot.Out]] =
    new ConstOutputType[W, C[O]] {
      override type Out = C[sot.Out]
      override def wrapConst(value: C[O]): C[sot.Out] = {
        val wrappedConst = wrapConstW.wrapConst(value)
        value.mapWithIndex { (a, idx) =>
          sot.wrapSelected(wrappedConst, DataPath.empty.atIndex(idx), a)
        }
      }
    }

  def selectOption[I : OP, O : OP](
    sot: SelectOutputType[W, I, O],
  ): SelectOutputType.Aux[W, I, Option[O], Option[sot.Out]] = selectTraverse[Option, I, O](sot)

  def selectTraverse[C[_] : Traverse, I : OP, O : OP](
    sot: SelectOutputType[W, I, O],
  ): SelectOutputType.Aux[W, I, C[O], C[sot.Out]] =
    new SelectOutputType[W, I, C[O]] {
      override type Out = C[sot.Out]
      override def wrapSelected(
        wrapped: W[I],
        path: DataPath,
        value: C[O],
      ): C[sot.Out] = {
        value.mapWithIndex { (a, i) =>
          sot.wrapSelected(wrapped, path.atIndex(i), a)
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
        path: DataPath,
        value: O,
      ): W[O] = wrapElementW.wrapSelected(wrapped, path, value)
    }
}
