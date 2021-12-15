package com.rallyhealth.vapors.v1

package dsl

import algebra.{Expr, Extract}
import com.rallyhealth.vapors.v1.lens.VariantLens.FromTo
import lens.VariantLens
import shapeless.Id

import scala.collection.Factory

/**
  * Captures the types of an expression to be applied in a subsequent builder method.
  *
  * This is a common pattern used when you want to call a method with a single explicit type parameter
  * but the method requires more than one type parameter.
  *
  * @tparam W the wrapper type (or effect)
  * @tparam A the output of the input expression
  * @tparam C the type of container to get as (captured by [[BuildExprDsl.SelectExprBuilder.getAs]]
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
class GetAsWrapper[-I, W[+_] : Extract, A, C[_], OP[_]](
  inputExpr: Expr[I, W[A], OP],
)(implicit
  ws: WrapSelected[W, OP],
) {

  def apply[NF[b] <: IterableOnce[b], B](
    selector: VariantLens.FromTo[A, NF[B]],
  )(implicit
    factory: Factory[W[B], C[W[B]]],
    opA: OP[A],
    opB: OP[B],
    opO: OP[C[W[B]]],
  ): Expr.Select[I, W[A], NF[B], C[W[B]], OP] = {
    val lens = VariantLens.id[W[A]].extractValue.andThen(selector(VariantLens.id[A]))
    Expr.Select[I, W[A], NF[B], C[W[B]], OP](inputExpr, lens, { (wa, cb) =>
      cb.iterator
        .map(ws.wrapSelected(wa, lens.path, _))
        .to(factory)
    })
  }
}

final class GetAsUnwrapped[-I, A, C[_], OP[_]](inputExpr: Expr[I, A, OP])
  extends GetAsWrapper[I, Id, A, C, OP](inputExpr) {

  override def apply[NF[b] <: IterableOnce[b], B](
    selector: FromTo[A, NF[B]],
  )(implicit
    factory: Factory[B, C[B]],
    opA: OP[A],
    opB: OP[B],
    opO: OP[C[B]],
  ): Expr.Select[I, A, NF[B], C[B], OP] = super.apply[NF, B](selector)
}
