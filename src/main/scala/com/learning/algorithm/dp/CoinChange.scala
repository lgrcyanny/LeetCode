package com.learning.algorithm.dp

object CoinChange {

  /**
   * exponential time
   */
  def getMinChange(coins: Array[Int], s: Int): Int = {
    def recur(xs: Int): Int = {
      if (xs == 0) {
        0
      } else {
        var res = Int.MaxValue
        for (i <- 0 until coins.size) {
          if (xs >= coins(i)) {
            val minCoins = recur(xs - coins(i))
            if (minCoins + 1 < res) {
              res = minCoins + 1
            }
          }
        }
        res
      }
    }
    recur(s)
  }

  def getMinChangeDP(coins: Array[Int], s: Int): Int = {
    val memo = Array.ofDim[Int](s + 1)
    memo(0)  = 0
    for (i <- 1 to s) {
      memo(i) = Int.MaxValue
    }
    for (i <- 1 to s) {
      for (j <- 0 until coins.size) {
        if (coins(j) <= i) {
          val tempCoins = memo(i - coins(j))
          if (tempCoins != Int.MaxValue && (tempCoins + 1 < memo(i))) {
            memo(i) = tempCoins + 1
          }
        }
      }
    }
    memo(s)
  }

  def main(args: Array[String]): Unit = {
    val coins = Array(1, 2, 5, 10, 20, 50, 12)
    val s = 65
    println(getMinChangeDP(coins, s))
  }

}
