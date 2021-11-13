package com.learning.algorithm.dp2.linear.stock

object LimitStockProfit {

  /**
   * max trade limit 2
   */
  def maxProfit(prices: Array[Int]): Int = {
    val n = prices.size
    var dp1 = -prices(0) // first buy
    var dp2 = 0 // sell after first buy
    var dp3 = -prices(0) // buy after finish first trade
    var dp4 = 0 // sell after second by
    for (i <- 1 until n) {
      dp1 = Math.max(dp1, -prices(i))
      dp2 = Math.max(dp2, dp1 + prices(i))
      dp3 = Math.max(dp3, dp2 - prices(i))
      dp4 = Math.max(dp4, dp3 + prices(i))
    }
    dp4
  }

  def maxProfitWithLimit(K: Int, prices: Array[Int]): Int = {
    val n = prices.size
    if (n <= 1) {
      0
    } else {
      val S = 2 * K
      val dp = Array.ofDim[Int](n, S + 1)
      for (s <- 1 to S) {
        if (s % 2 == 1) {
          dp(0)(s) = -prices(0) // buy stock
        } else {
          dp(0)(s) = 0 // sell stock
        }
      }
      for (i <- 1 until n) {
        for (s <- 1 to S) {
          if (s % 2 == 1) { // buy
            dp(i)(s) = Math.max(dp(i - 1)(s), dp(i - 1)(s - 1) - prices(i))
          } else { // sell
            dp(i)(s) = Math.max(dp(i - 1)(s), dp(i - 1)(s - 1) + prices(i))
          }
        }
      }
      dp(n - 1)(S)
    }
  }

  def main(args: Array[String]): Unit = {
    val prices = Array(3, 3, 5, 0, 0, 3, 1, 4)
    println(maxProfit(prices))
    println(maxProfitWithLimit(2, prices))
  }

}
