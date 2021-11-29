package com.learning.algorithm.dp2.linear.matrix

object Dungeon {

  def calculateMinimumHP(dungeon: Array[Array[Int]]): Int = {
    val m = dungeon.size
    val n = dungeon(0).size
    val dp = Array.ofDim[Int](m + 1, n + 1)
    for (i <- 0 to m) {
      dp(i)(n) = Int.MaxValue
    }
    for (j <- 0 to n) {
      dp(m)(j) = Int.MaxValue
    }
    dp(m)(n - 1) = 1
    dp(m - 1)(n) = 1
    for (i <- m - 1 to 0 by -1) {
      for (j <- n - 1 to 0 by -1) {
        dp(i)(j) = Math.max(1, Math.min(dp(i + 1)(j), dp(i)(j + 1)) - dungeon(i)(j))
      }
    }
    dp(0)(0)
  }

  def main(args: Array[String]): Unit = {
    val dungeon = Array(Array(-2, -3, 3), Array(-5, -10, 1), Array(10, 30, -5))
    println(calculateMinimumHP(dungeon))
  }

}
