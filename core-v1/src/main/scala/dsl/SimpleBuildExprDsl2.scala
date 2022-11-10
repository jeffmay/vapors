package com.rallyhealth.vapors.v1
package dsl

import algebra.Expr
import debug.HasShow

class SimpleBuildExprDsl2 {

  type W[+O] = O

  type ConstW[+I] = I

  type OP[O] = HasShow[O]

  protected def wrapParam: WrapParam[W, OP] = WrapParam.wrapID
  
  given param[O : OP]: OP[W[O]] = wrapParam.wrapParam[O]

  extension [O : OP] (value: O) inline def const: Expr.Const[O, OP] =
    Expr.Const(value)

}
