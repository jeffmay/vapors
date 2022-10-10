package com.rallyhealth.vapors.v2
package dsl

import debug.HasShow

import zio.UIO

/**
 * Import from this package to get the "simple" DSL which computes everything synchronously on the calling thread
 * and returns an unwrapped result.
 *
 * @note the Scala 3 idiomatic approach for this would be to export the methods of some [[SimpleDsl]] object,
 *       rather than use a package object, however, some editors are unable to trace code back to the source
 *       from package exports. Once this is fixed we can transition this to an export statement.
 */
package object simple extends SimpleDsl {
  override type OP[a] = HasShow[a]
  override type F[a] = a

  object uio extends SimpleDsl {
    override type OP[a] = HasShow[a]
    override type F[a] = UIO[a]
  }
}
