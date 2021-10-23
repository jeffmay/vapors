//package com.rallyhealth.vapors.v1
//
//import algebra.Expr
//
//import com.rallyhealth.vapors.v1.data.Justified
//
//import scala.annotation.nowarn
//
//trait CommonJustifiedLogicalSpec extends BaseDslSpec {
//
//  import thisDsl._
//
//  def justify[C[_], A, B](@nowarn expr: Expr.Exists[C, A, B, OP]): JustifyExists[C, A, B, Nothing, Nothing] =
//    new JustifyExists(None, None)
//
//  final class JustifyExists[C[_], A, B, IA, IB](
//    originalValue: Option[C[IA]],
//    expectedValue: Option[IB],
//  ) {
//
//    def withCondition(conditionInfo: String): Justified[Boolean]
//  }
//}
