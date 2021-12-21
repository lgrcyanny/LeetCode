package com.learning.algorithm.dp2.count.catalan

object NumTree {

  def numTrees(n: Int): Int = {
    val dp = Array.ofDim[Int](n + 1)
    dp(0) = 1
    dp(1) = 1
    for (i <- 2 to n) {
      for (j <- 1 to i) {
        dp(i) = dp(i) + dp(j - 1) * dp(i - j)
      }
    }
    dp(n)
  }

  def main(args: Array[String]): Unit = {
    println(numTrees(3))
  }

}
