package com.rallyhealth.vapors.v2
package optical

type Extracting[+B] = [a] =>> ExtractValue[a, B]
object Extracting {
  inline def apply[B]: As[B] = As[B]

  sealed abstract class As[-B]
  case object As extends As[Any] {
    def apply[B]: As[B] = this.asInstanceOf[As[B]]
  }

  extension [B] (as: As[B]) def from[A](using extractor: ExtractValue[A, B]): ExtractValue[A, B] = extractor

  extension [A](source: A) def extract[B](using extractor: ExtractValue[A, B]): B = extractor.extract(source)
}
