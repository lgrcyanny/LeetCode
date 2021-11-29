package com.learning.algorithm.dp2.linear.matrix

object MinFallingPathSum {

  def minFallingPathSum(matrix: Array[Array[Int]]): Int = {
    val n = matrix.size
    val dp = Array.ofDim[Int](n, n)
    for (i <- n - 1 to 0 by -1) {
      for (j <- 0 until n) {
        if (i == n - 1) {
          dp(i)(j) = matrix(i)(j)
        } else if (j == 0) {
          dp(i)(j) = Math.min(dp(i + 1)(0), dp(i + 1)(1)) + matrix(i)(j)
        } else if (j == n - 1) {
          dp(i)(j) = Math.min(dp(i + 1)(n - 2), dp(i + 1)(n - 1)) + matrix(i)(j)
        } else {
          dp(i)(j) = Array(dp(i + 1)(j), dp(i + 1)(j - 1), dp(i + 1)(j + 1)).min + matrix(i)(j)
        }
      }
    }
    dp(0).min
  }

  def main(args: Array[String]): Unit = {
//    val matrix = Array(Array(2, 1, 3), Array(6, 5, 4), Array(7, 8, 9))
    val matrix = Array(Array(17, 82), Array(1, -44))
    println(minFallingPathSum(matrix))
  }

}
