package com.rallyhealth.vapors.v1

package lens

object JavaBeanCompat {

  // originally this contained both "is" and "get", but I think x.isEmpty reads better than x.empty
  private final val GETTER_PREFIXES = Set("get")

  def unbeanify(name: String): String = {
    val unbeanified = GETTER_PREFIXES.collectFirst(Function.unlift { prefix =>
      Option
        .when(name.startsWith(prefix) && name.length > prefix.length) {
          val firstChar = name.charAt(prefix.length)
          Option.when(firstChar.isUpper) {
            val restOfName = name.substring(prefix.length + 1)
            s"${firstChar.toLower}$restOfName"
          }
        }
        .flatten
    })
    unbeanified.getOrElse(name)
  }
}
