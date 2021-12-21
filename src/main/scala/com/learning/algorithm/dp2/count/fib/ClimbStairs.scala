package com.learning.algorithm.dp2.count.fib

object ClimbStairs {

  def climbStairs(n: Int): Int = {
    if (n <= 1) {
      1
    } else {
      var p = 1 // dp[0]
      var q = 1 // dp[1]
      for (i <- 2 to n) {
        val t = q
        q = p + q
        p = t
      }
      q
    }
  }

  def main(args: Array[String]): Unit = {
    println(climbStairs(4))
  }
}
