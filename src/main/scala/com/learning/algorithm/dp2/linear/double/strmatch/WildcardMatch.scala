package com.learning.algorithm.dp2.linear.double.strmatch

object WildcardMatch {

  def isMatch(s: String, p: String): Boolean = {
    val m = s.size
    val n = p.size
    val dp = Array.ofDim[Boolean](m + 1, n + 1)
    for (i <- 0 to m) {
      for (j <- 0 to n) {
        dp(i)(j) = false
      }
    }
    dp(0)(0) = true
    for (j <- 1 to n) {
      if (p(j - 1) == '*') {
        dp(0)(j) = dp(0)(j - 1)
      }
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (s(i - 1) == p(j - 1) || p(j - 1) == '?') {
          dp(i)(j) = dp(i - 1)(j - 1)
        } else if (p(j - 1) == '*') {
          dp(i)(j) = dp(i - 1)(j) || dp(i)(j - 1)
        }
      }
    }
    dp(m)(n)
  }

  def main(args: Array[String]): Unit = {
    val s = "acdcb"
    val p = "a*c?b"
    println(isMatch(s, p))
  }

}
