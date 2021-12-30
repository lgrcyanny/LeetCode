package com.learning.algorithm.dp2.knapstack

object CoinChange {

  /**
   * It's not right
   */
  def coinChange(coins: Array[Int], amount: Int): Int = {
    val n = coins.size
    val dp = Array.ofDim[Int](n, amount + 1)
    for (i <- 0 until n) {
      for (j <- 0 to amount) {
        if (j == 0) {
          dp(i)(j) = 0
        } else {
          dp(i)(j) = Int.MaxValue - 1
        }
      }
    }
    for (i <- 0 until n) {
      for (j <- 1 to amount) {
          if (j >= coins(i)) {
            dp(i)(j) = Math.min(dp(i)(j), dp(i)(j - coins(i)) + 1)
            if (i > 0) {
              dp(i)(j) = Math.min(dp(i - 1)(j), dp(i)(j - coins(i)) + 1)
            }
          }
      }
    }
    val res = dp(n - 1)(amount)
    if (res > amount) -1 else res
  }

  def coinChangeOpt(coins: Array[Int], amount: Int): Int = {
    val n = coins.size
    val dp = Array.ofDim[Int](amount + 1)
    dp(0) = 0
    for (j <- 1 to amount) {
      dp(j) = Int.MaxValue - 1
    }
    for (i <- 0 until n) {
      for (j <- 0 to amount) {
        if (coins(i) <= j) {
          dp(j) = Math.min(dp(j), dp(j - coins(i)) + 1)
        }
      }
    }
    val res = dp(amount)
    if (res > amount) -1 else res
  }

  def main(args: Array[String]): Unit = {
    val coins = Array(1, 2, 5)
    val amount = 11
    println(coinChangeOpt(coins, amount))
  }

}
