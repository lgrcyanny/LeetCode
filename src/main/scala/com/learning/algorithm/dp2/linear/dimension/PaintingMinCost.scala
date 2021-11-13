package com.learning.algorithm.dp2.linear.dimension

object PaintingMinCost {

  def minCost(costs: Array[Array[Int]]): Int = {
    val N = costs.size
    val K = costs(0).size
    val dp = Array.ofDim[Int](N, K)
    for (k <- 0 until K) {
      dp(0)(k) = costs(0)(k)
    }
    for (i <- 1 until N) {
      for (k <- 0 until K) {
        dp(i)(k) = Int.MaxValue
        for (m <- 0 until K if m != k) {
          dp(i)(k) = Math.min(dp(i)(k), dp(i - 1)(m) + costs(i)(k))
        }
      }
    }
    val t = for (k <- 0 until K) yield dp(N - 1)(k)
    val res = t.min
    res
  }

  def main(args: Array[String]): Unit = {
    val costs = Array(Array(17, 2, 17), Array(16, 16, 5), Array(14, 3, 19))
    println(minCost(costs))
  }

}
