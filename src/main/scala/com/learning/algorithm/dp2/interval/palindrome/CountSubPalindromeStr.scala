package com.learning.algorithm.dp2.interval.palindrome

object CountSubPalindromeStr {

  def countSubstrings(s: String): Int = {
    val n = s.size
    val dp = Array.ofDim[Boolean](n, n)
    var count = 0
    for (len <- 1 to n) {
      for (i <- 0 until n) {
        val j = i + len - 1
        if (j < n) {
          if (i == j) {
            dp(i)(j) = true
          } else if (s(i) != s(j)) {
            dp(i)(j) = false
          } else {
            if (j - i < 3) {
              dp(i)(j) = true // such as "aca"
            } else {
              dp(i)(j) = dp(i + 1)(j - 1)
            }
          }
          if (dp(i)(j)) {
            count = count + 1
          }
        }
      }
    }
    count
  }

  def main(args: Array[String]): Unit = {
    val s = "abc"
    println(countSubstrings(s))
  }

}
