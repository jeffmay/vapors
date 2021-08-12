//package com.rallyhealth
//
//package vapors.v1.engine
//
//import vapors.v1.algebra.ExprResult
//import vapors.v1.engine.InterpretExprResultAsString.Serialize
//
//import cats.{Foldable, Id}
//
//object InterpretExprResultAsString {
//  type Serialize[-I, +O] = String
//}
//
//class InterpretExprResultAsString[+PO] extends ExprResult.Visitor[Id, PO, InterpretExprResultAsString.Serialize] {
//
//  override def visitCombine[I >: PO, RL, RR, O](result: ExprResult.Combine[Id, I, PO, RL, RR, O]): Serialize[I, O] = ???
//
//  override def visitConst[O](result: ExprResult.Const[Id, O]): Serialize[Any, O] = ???
//
//  override def visitExists[I >: PO, C[_] : Foldable, E](
//    result: ExprResult.Exists[Id, I, PO, C, E],
//  ): Serialize[I, Boolean] = ???
//
//  override def visitForAll[I >: PO, C[_] : Foldable, E](
//    result: ExprResult.ForAll[Id, I, PO, C, E],
//  ): Serialize[I, Boolean] = ???
//
//  override def visitIdentity[I >: PO](result: ExprResult.Identity[Id, I]): Serialize[I, I] = ???
//
//  override def visitWithFactValues[T, O](result: ExprResult.WithFactValues[Id, T, O]): Serialize[Any, O] = ???
//}
