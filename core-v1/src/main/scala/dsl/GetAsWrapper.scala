package com.rallyhealth.vapors.v1

package dsl

import algebra.{Expr, Extract}
import lens.{DataPath, VariantLens}

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
final class GetAsWrapper[-I, W[+_] : Extract : WrapConst, A, C[_], OP[_]](
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
  ): Expr.Select[I, W, A, NF[B], C[W[B]], OP] = {
    implicit val sot: SelectOutputType.Aux[W, A, NF[B], C[W[B]]] =
      new SelectOutputType[W, A, NF[B]] {
        override type Out = C[W[B]]
        override def wrapSelected(
          wrapped: W[A],
          path: DataPath,
          value: NF[B],
        ): C[W[B]] = {
          value.iterator
            .map { b =>
              ws.wrapSelected(wrapped, path, b)
            }
            .to(factory)
        }
      }
    val lens = selector(VariantLens.id[A])
    Expr.Select[I, W, A, NF[B], C[W[B]], OP](inputExpr, lens, sot.wrapSelected(_, lens.path, _))
  }
}
