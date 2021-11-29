package com.learning.algorithm.dp2.linear.matrix

object MaxSquare {
  def maximalSquare(matrix: Array[Array[Char]]): Int = {
    val m = matrix.size
    val n = matrix(0).size
    val dp = Array.ofDim[Int](m, n)
    var res = 0
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (matrix(i)(j) == '1') {
          if (i == 0 || j == 0) {
            dp(i)(j) = 1
          } else {
            dp(i)(j) = Array(dp(i - 1)(j), dp(i)(j - 1), dp(i - 1)(j - 1)).min + 1
          }
        } else {
          dp(i)(j) = 0
        }
        res = Math.max(res, dp(i)(j))
      }
    }
    res * res
  }

  def main(args: Array[String]): Unit = {
    val matrix = Array(
      Array("1", "0", "1", "0", "0"),
      Array("1", "0", "1", "1", "1"),
      Array("1", "1", "1", "1", "1"),
      Array("1", "0", "0", "1", "0")
    ).map(arr => arr.map(_.toCharArray.head))
    println(maximalSquare(matrix))
  }
}
