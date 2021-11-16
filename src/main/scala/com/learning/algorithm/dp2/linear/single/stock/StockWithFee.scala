package com.learning.algorithm.dp2.linear.single.stock

object StockWithFee {

  def maxProfit(prices: Array[Int], fee: Int): Int = {
    val n = prices.size
    var dp0 = 0
    var dp1 = -prices(0)
    for (i <- 1 until n) {
      dp0 = Math.max(dp0, dp1 + prices(i) - fee)
      dp1 = Math.max(dp1, dp0 - prices(i))
    }
    dp0
  }

  def main(args: Array[String]): Unit = {
//    val prices = Array(1, 3, 2, 8, 4, 9) // expect 8
    val prices = Array(1, 3, 7, 5, 10, 3)
    println(maxProfit(prices, 3))
  }

}
