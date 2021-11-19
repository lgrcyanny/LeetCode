package com.learning.algorithm.dp2.linear.double.strmatch

object EditDistance {

  def minDistance(word1: String, word2: String): Int = {
    val m = word1.size
    val n = word2.size
    val dp = Array.ofDim[Int](m + 1, n + 1)
    for (i <- 0 to m) {
      dp(i)(0) = i
    }
    for (j <- 0 to n) {
      dp(0)(j) = j
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (word1(i - 1) == word2(j - 1)) {
          dp(i)(j) = dp(i - 1)(j - 1)
        } else {
          dp(i)(j) = Array(dp(i - 1)(j), dp(i)(j - 1), dp(i - 1)(j - 1)).min + 1 // delete, insert, update cost
        }
      }
    }
    dp(m)(n)
  }

  def main(args: Array[String]): Unit = {
    val word1 = "intention"
    val word2 = "execution"
    println(minDistance(word1, word2))
  }

}
