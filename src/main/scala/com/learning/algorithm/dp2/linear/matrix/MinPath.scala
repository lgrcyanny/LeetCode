package com.learning.algorithm.dp2.linear.matrix

object MinPath {

  def minPathSum(grid: Array[Array[Int]]): Int = {
    val m = grid.size
    val n = grid(0).size
    val dp = Array.ofDim[Int](m, n)
    dp(0)(0) = grid(0)(0)
    for (i <- 1 until m) {
      dp(i)(0) = grid(i)(0) + dp(i - 1)(0)
    }
    for (j <- 1 until n) {
      dp(0)(j) = grid(0)(j) + dp(0)(j - 1)
    }
    for (i <- 1 until m) {
      for (j <- 1 until n) {
        dp(i)(j) = Math.min(dp(i - 1)(j), dp(i)(j - 1)) + grid(i)(j)
      }
    }
    dp(m - 1)(n - 1)
  }

  def main(args: Array[String]): Unit = {
    val grid = Array(Array(1, 3, 1), Array(1, 5, 1), Array(4, 2, 1))
    println(minPathSum(grid))
  }

}
