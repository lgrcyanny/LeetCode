package com.learning.algorithm.dp2.interval.palindrome

object LongestPalindromeSubSeq {

  def longestPalindromeSubseq(s: String): Int = {
    val n = s.size
    val dp = Array.ofDim[Int](n, n) // it's sparse, can replace with hashmap
    for (len <- 1 to n) {
      for (i <- 0 until n) {
        val j = i + len - 1
        if (j < n) {
          if (i == j) {
            dp(i)(j) = 1
          } else if (s(i) != s(j)) {
            dp(i)(j) = Math.max(dp(i + 1)(j), dp(i)(j - 1))
          } else {
            dp(i)(j) = dp(i + 1)(j - 1) + 2
          }
        }
      }
    }
    dp(0)(n - 1)
  }

  def main(args: Array[String]): Unit = {
    val s = "bbab"
    println(longestPalindromeSubseq(s))
  }
}
