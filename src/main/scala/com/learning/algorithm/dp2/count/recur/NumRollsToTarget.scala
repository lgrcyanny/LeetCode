package com.learning.algorithm.dp2.count.recur

object NumRollsToTarget {

  def numRollsToTarget(n: Int, f: Int, target: Int): Int = {
    val mod = (1e9 + 7).toInt
    val dp = Array.ofDim[Int](target + 1)
    dp(0) = 1
    for (i <- 1 to n) {
      for (j <- target to 0 by -1) {
        dp(j) = 0
        for (k <- 1 to f if j >= k) {
          dp(j) = (dp(j) + dp(j - k)) % mod
        }
      }
    }
    dp(target)
  }

  def main(args: Array[String]): Unit = {
    println(numRollsToTarget(2, 5, 10))
  }
}
