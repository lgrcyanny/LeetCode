package com.learning.algorithm.dp2.linear.double.lcs

object MinAsciiDeleteSum {

  def minimumDeleteSum(s1: String, s2: String): Int = {
    val m = s1.size
    val n = s2.size
    val dp = Array.ofDim[Int](m + 1, n + 1)
    val s1Arr = s1.toCharArray.map(_.toInt)
    val s2Arr = s2.toCharArray.map(_.toInt)
    dp(0)(0) = 0
    for (i <- 1 to m) {
      dp(i)(0) = s1Arr.slice(0, i).sum
    }
    for (j <- 1 to n) {
      dp(0)(j) = s2Arr.slice(0, j).sum
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (s1(i - 1) == s2(j - 1)) {
          dp(i)(j) = dp(i - 1)(j - 1)
        } else {
          dp(i)(j) = Math.min(dp(i - 1)(j) + s1Arr(i - 1), dp(i)(j - 1) + s2Arr(j - 1))
        }
      }
    }
    dp(m)(n)
  }

  def main(args: Array[String]): Unit = {
    // expect 231
//    val s1 = "sea"
//    val s2 = "eat"
    // expect 403
    val s1 = "delete"
    val s2 = "leet"
    println(minimumDeleteSum(s1, s2))
  }
}
