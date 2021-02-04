package com.rallyhealth.vapors.core.dsl

import cats.{Monad, Traverse, TraverseFilter}

trait ExprBuilderCatsInstances {

  implicit def monadSet: Monad[Set] = alleycats.std.set.alleyCatsStdSetMonad

  implicit def traverseSet: Traverse[Set] = alleycats.std.set.alleyCatsSetTraverse

  implicit def traverseFilterSet: TraverseFilter[Set] = alleycats.std.set.alleyCatsSetTraverseFilter

  implicit def traverseMap[K]: Traverse[Map[K, *]] = alleycats.std.map.alleycatsStdInstancesForMap

  implicit def traverseFilterMap[K]: TraverseFilter[Map[K, *]] = alleycats.std.map.alleycatsStdMapTraverseFilter
}
