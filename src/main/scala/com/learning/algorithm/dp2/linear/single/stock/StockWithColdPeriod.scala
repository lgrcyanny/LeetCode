package com.learning.algorithm.dp2.linear.single.stock

object StockWithColdPeriod {

  def maxProfit(prices: Array[Int]): Int = {
    val N = prices.size
    val S = 3
    val dp = Array.ofDim[Int](N, S)
    dp(0)(0) = -prices(0) // buy one stock
    dp(0)(1) = 0 // have no stock, not in cold period
    dp(0)(2) = 0 // have no stock, in cold period
    for (i <- 1 until N) {
      dp(i)(0) = Math.max(dp(i - 1)(0), dp(i - 1)(1) - prices(i))
      dp(i)(1) = Math.max(dp(i - 1)(1), dp(i - 1)(2))
      dp(i)(2) = dp(i - 1)(0) + prices(i)
    }
    Math.max(dp(N - 1)(1), dp(N - 1)(2))
  }

  def main(args: Array[String]): Unit = {
    val prices = Array(1, 2, 3, 0, 2)
    println(maxProfit(prices))
  }

}
