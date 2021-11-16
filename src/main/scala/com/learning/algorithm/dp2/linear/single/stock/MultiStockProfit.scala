package com.learning.algorithm.dp2.linear.single.stock

object MultiStockProfit {

  def maxProfit(prices: Array[Int]): Int = {
    val n = prices.size
    var dp0 = 0
    var dp1 = -prices(0)
    for (i <- 1 until n) {
      dp0 = Math.max(dp0, dp1 + prices(i))
      dp1 = Math.max(dp1, dp0 - prices(i))
    }
    dp0
  }

  def main(args: Array[String]): Unit = {
    val prices = Array(7, 1, 5, 3, 6, 4)
    println(maxProfit(prices))
  }

}
