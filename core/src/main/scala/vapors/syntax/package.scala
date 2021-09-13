package com.rallyhealth

package vapors

package object syntax {

  type IndexedSyntax = vapors.v1.lens.IndexedSyntax

  object all extends IndexedSyntax with MathSyntax

  object indexed extends IndexedSyntax

  object math extends MathSyntax
}
