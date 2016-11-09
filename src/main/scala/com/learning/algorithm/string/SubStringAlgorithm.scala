package com.learning.algorithm.string

/**
  * Created by lgrcyanny on 16/11/9.
  */
object SubStringAlgorithm {
  def strstr(str: String, subString: String): Int = {
    def indexOf(fromIndex: Int, endIndex: Int): Int = {
      var findIndex = -1
      var i = fromIndex
      while (findIndex < 0 && i <= endIndex) {
        if (str(i) == subString(0)) {
          var srcIndex = i
          var subIndex = 0
          while (srcIndex <= endIndex
            && subIndex < subString.length
            && str(srcIndex) == subString(subIndex)) {
            srcIndex += 1
            subIndex += 1
          }
          if (subIndex == subString.length) {
            findIndex = i
          }
        }
        i += 1
      }
      findIndex
    }
    indexOf(0, str.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val str = "abcdef"
    val standardIndex = str.indexOf("def")
    println(standardIndex)
    val myIndex = strstr(str, "def")
    println(myIndex)
  }

}
