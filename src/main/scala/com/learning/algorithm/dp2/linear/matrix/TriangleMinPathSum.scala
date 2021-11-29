package com.learning.algorithm.dp2.linear.matrix

object TriangleMinPathSum {

  def minimumTotal(triangle: List[List[Int]]): Int = {
    val n = triangle.size
    val dp = Array.ofDim[Int](n, n)
    dp(0)(0) = triangle(0)(0)
    for (i <- 1 until n) {
      for (j <- 0 to i) {
        if (j == 0) {
          dp(i)(j) = dp(i - 1)(j) + triangle(i)(j)
        } else if (j == i) {
          dp(i)(j) = dp(i - 1)(j - 1) + triangle(i)(j)
        } else {
          dp(i)(j) = Math.min(dp(i - 1)(j), dp(i - 1)(j - 1)) + triangle(i)(j)
        }
      }
    }
    val res = dp(n - 1).min
    res
  }

  def minimumTotalOpt(triangle: List[List[Int]]): Int = {
    val n = triangle.size
    val dp = Array.ofDim[Int](n)
    dp(0) = triangle(0)(0)
    for (i <- 1 until n) {
      dp(i) = dp(i - 1) + triangle(i)(i)
      for (j <- i - 1 to 1 by -1) {
        dp(j) = Math.min(dp(j), dp(j - 1)) + triangle(i)(j)
      }
      dp(0) = dp(0) + triangle(i)(0)
    }
    dp.min
  }

  def main(args: Array[String]): Unit = {
    val triangle = List(List(2), List(3, 4), List(6, 5, 7), List(4, 1, 8, 3))
    println(minimumTotal(triangle))
    println(minimumTotalOpt(triangle))
  }

}
