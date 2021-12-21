package com.learning.algorithm.dp2.count.path

object UniquePathObstacles {

  def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {
    val m = obstacleGrid.size
    val n = obstacleGrid(0).size
    val dp = Array.ofDim[Int](m, n)
    if (obstacleGrid(0)(0) == 0) {
      dp(0)(0) = 1
    }
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (obstacleGrid(i)(j) == 0) {
          if (i > 0 && obstacleGrid(i - 1)(j) == 0) {
            dp(i)(j) = dp(i)(j) + dp(i - 1)(j)
          }
          if (j > 0 && obstacleGrid(i)(j - 1) == 0) {
            dp(i)(j) = dp(i)(j) + dp(i)(j - 1)
          }
        }
      }
    }
    dp(m - 1)(n - 1)
  }

  def uniquePathsWithObstaclesOpt(obstacleGrid: Array[Array[Int]]): Int = {
    val m = obstacleGrid.size
    val n = obstacleGrid(0).size
    val dp = Array.ofDim[Int](n)
    if (obstacleGrid(0)(0) == 0) {
      dp(0) = 1
    }
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (obstacleGrid(i)(j) == 0) {
          if (i > 0 && obstacleGrid(i - 1)(j) == 1) {
            dp(j) = 0
          }
          if (j > 0 && obstacleGrid(i)(j - 1) == 0) {
            dp(j) = dp(j) + dp(j - 1)
          }
        } else {
          dp(j) = 0
        }
      }
    }
    dp(n - 1)
  }

  def main(args: Array[String]): Unit = {
    val grid = Array(
      Array(0, 0, 0),
      Array(0, 1, 0),
      Array(0, 0, 0))
    println(uniquePathsWithObstaclesOpt(grid))
  }

}
