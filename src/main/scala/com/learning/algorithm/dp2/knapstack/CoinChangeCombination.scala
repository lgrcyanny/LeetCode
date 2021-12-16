package com.learning.algorithm.dp2.knapstack

object CoinChangeCombination {
  def change(amount: Int, coins: Array[Int]): Int = {
    val n = coins.size
    val dp = Array.ofDim[Int](amount + 1)
    dp(0) = 1
    for (i <- 0 until n) {
      for (j <- 1 to amount) {
        if (j >= coins(i)) {
          dp(j) = dp(j) + dp(j - coins(i))
        }
      }
    }
    dp(amount)
  }

  def main(args: Array[String]): Unit = {
    val coins = Array(1, 2, 5)
    val amount = 5
    println(change(amount, coins))
  }
}
