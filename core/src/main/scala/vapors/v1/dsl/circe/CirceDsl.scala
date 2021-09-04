package com.rallyhealth

package vapors.v1.dsl.circe

import vapors.v1.algebra.Expr

import io.circe.Encoder

trait CirceDsl {

  def const[O : Encoder](value: O): Expr.Const[O, Encoder] = Expr.Const(value)
}
