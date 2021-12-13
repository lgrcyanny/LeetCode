package com.learning.algorithm.dp2.interval.palindrome

object LongestDecomposition {

  def longestDecomposition(text: String): Int = {
    val n = text.size
    var left = 0
    var right = n - 1
    var len = 1
    var count = 0
    while (left < right && len <= right - left) {
      val leftStr = text.substring(left, left + len)
      val rightStr = text.substring(right - len + 1, right + 1)
      if (leftStr == rightStr) {
        count = count + 2
        left = left + len
        right = right - len
        len = 1
      } else {
        len = len + 1
      }
    }
    if (left == right + 1) {// even length
      count
    } else { // odd length
      count + 1
    }
  }

  def main(args: Array[String]): Unit = {
    val text = "ghiabcdefhelloadamhelloabcdefghi"
    println(longestDecomposition(text))
  }

}
