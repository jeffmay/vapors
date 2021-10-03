package com.rallyhealth.vapors.v1

package lens

object JavaBeanCompat {

  // originally this contained both "is" and "get", but I think x.isEmpty reads better than x.empty
  private final val GETTER_PREFIXES = Set("get")

  def unbeanify(name: String): String = {
    for (prefix <- GETTER_PREFIXES) {
      if (name.startsWith(prefix) && name.length > prefix.length) {
        val firstChar = name.charAt(prefix.length)
        if (firstChar.isUpper) {
          val restOfName = name.substring(prefix.length + 1)
          return s"${firstChar.toLower}$restOfName"
        }
      }
    }
    name
  }
}
