package com.learning.algorithm.dp2.linear.double.lcs

object LongestCommonSubSeq {
  def longestCommonSubsequence(text1: String, text2: String): Int = {
    val m = text1.size
    val n = text2.size
    val dp = Array.ofDim[Int](m + 1, n + 1)
    for (i <- 0 to m) {
      dp(i)(0) = 0
    }
    for (j <- 0 to n) {
      dp(0)(j) = 0
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (text1(i - 1) == text2(j - 1)) {
          dp(i)(j) = dp(i - 1)(j - 1) + 1
        } else {
          dp(i)(j) = Math.max(dp(i - 1)(j), dp(i)(j - 1))
        }
      }
    }
    println(printLCS(text1, text2, dp))
    dp(m)(n)
  }

  def printLCS(text1: String, text2: String, lcsCount: Array[Array[Int]]): String = {
    val seq = new StringBuilder
    val m = text1.size
    val n = text2.size
    var i = m
    var j = n
    while (i > 0 && j > 0) {
      if (text1(i - 1) == text2(j - 1)) {
        seq.append(text1(i - 1))
        i = i - 1
        j = j - 1
      } else if (lcsCount(i - 1)(j) >= lcsCount(i)(j - 1)){
        i = i - 1
      } else {
        j = j - 1
      }
    }
    seq.reverse.toString()
  }

  def main(args: Array[String]): Unit = {
    val text1 = "abcde"
    val text2 = "ace"
    println(longestCommonSubsequence(text1, text2))
  }

}
