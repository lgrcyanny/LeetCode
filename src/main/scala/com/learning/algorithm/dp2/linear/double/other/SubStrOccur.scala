package com.learning.algorithm.dp2.linear.double.other

object SubStrOccur {

  def numDistinct(s: String, t: String): Int = {
    val m = s.size
    val n = t.size
    val dp = Array.ofDim[Int](m + 1, n + 1)
    for (j <- 0 to n) {
      dp(0)(j) = 0
    }
    for (i <- 0 to m) {
      dp(i)(0) = 1
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (s(i - 1) == t(j - 1)) {
          dp(i)(j) = dp(i - 1)(j - 1) + dp(i - 1)(j)
        } else {
          dp(i)(j) = dp(i - 1)(j)
        }
      }
    }
    dp(m)(n)
  }

  def numDistinctOpt(s: String, t: String): Int = {
    val m = s.size
    val n = t.size
    val dp = Array.ofDim[Int](n + 1)
    dp(0) = 1
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (s(i - 1) == t(j - 1)) {
          dp(j) = dp(j - 1) + dp(j)
        } else {
          dp(j) = dp(j - 1)
        }
      }
    }
    dp(n)
  }

  def main(args: Array[String]): Unit = {
    val s = "rabbbit"
    val t = "rabbit"
    println(numDistinct(s, t))
    println(numDistinctOpt(s, t)) // it's wrong, why?
  }

}
