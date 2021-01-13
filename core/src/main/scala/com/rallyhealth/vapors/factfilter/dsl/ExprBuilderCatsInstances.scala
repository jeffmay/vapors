package com.rallyhealth.vapors.factfilter.dsl

import cats.{Monad, Traverse, TraverseFilter}

trait ExprBuilderCatsInstances {

  implicit def monadSet: Monad[Set] = alleycats.std.set.alleyCatsStdSetMonad

  implicit def traverseSet: Traverse[Set] = alleycats.std.set.alleyCatsSetTraverse

  implicit def traverseFilterSet: TraverseFilter[Set] = alleycats.std.set.alleyCatsSetTraverseFilter
}
