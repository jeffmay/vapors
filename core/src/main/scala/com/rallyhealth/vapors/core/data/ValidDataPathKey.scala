package com.rallyhealth.vapors.core.data

trait ValidDataPathKey[K] {

  def stringify(key: K): String
}

object ValidDataPathKey {

  def apply[K](implicit valid: ValidDataPathKey[K]): ValidDataPathKey[K] = valid

  def instance[K <: Equals](f: K => String): ValidDataPathKey[K] = k => f(k)

  implicit val string: ValidDataPathKey[String] = identity[String]

  implicit val int: ValidDataPathKey[Int] = _.toString

  implicit val long: ValidDataPathKey[Long] = _.toString

}
