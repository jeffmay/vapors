package com.rallyhealth.vapors.circe

import cats.~>
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.data.DataPath
import com.rallyhealth.vapors.core.dsl._
import io.circe.Decoder.Result
import io.circe.{ACursor, Decoder, HCursor}

import scala.collection.BitSet

object CursorCodec {

  // TODO: Figure this out
//  private final class FoldToDecoder[T] extends (ExpAlg[T, *] ~> Decoder) {
//    override def apply[A](fa: ExpAlg[T, A]): Decoder[A] = Decoder.instance(decodeNode(fa, _))
//
//    private def decodeNode[A](
//      node: ExpAlg[T, A],
//      hc: HCursor,
//    ): Result[A] = {
//      node match {
//        case ExpSelectField(selector, exp) =>
//          // TODO: Move this somewhere easier to test
//          var c: ACursor = hc
//          for (n <- selector.path.nodes) {
//            n match {
//              case DataPath.Field(name) =>
//                c = c.downField(name)
//              case DataPath.Head =>
//                c = c.downArray
//              case DataPath.Last =>
//                c = c.downArray
//                val nSiblings = c.values.fold(0)(_.size)
//                for (_ <- 0 until nSiblings) {
//                  c = c.right
//                }
//              case DataPath.Idx(idx) =>
//                c = c.downN(idx)
//              case DataPath.IdxRange(range) =>
//                val siblingSet = BitSet.fromSpecific(range)
//                c = filterIndexes(c, siblingSet)
//              case DataPath.IdxSet(siblingSet) =>
//                c = filterIndexes(c, siblingSet)
//              case DataPath.MapKey(key) =>
//                c = c.downField(key)
//            }
//          }
//          c.as(exp.foldMap(new FoldToDecoder))
//      }
//    }
//
//    private def filterIndexes(
//      cursor: ACursor,
//      keep: BitSet,
//    ): ACursor = {
//      cursor.withFocus(_.mapArray(_.zipWithIndex.collect {
//        case (v, idx) if keep(idx) => v
//      }))
//    }
//  }
//
//  def decoderFromPath[T, A](path: AnyExp[T, A]): Decoder[A] = {
//    path.foldMap(new FoldToDecoder[T])
//  }

}
