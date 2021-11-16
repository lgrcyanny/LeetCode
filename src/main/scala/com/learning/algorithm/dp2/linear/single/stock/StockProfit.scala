package com.learning.algorithm.dp2.linear.single.stock

object StockProfit {

  /**
   * O(n^2)
   */
  def maxProfit(prices: Array[Int]): Int = {
    var profit = 0
    val n = prices.size
    for (i <- 0 until n) {
      for (j <- i + 1 until n) {
        profit = Math.max(profit, prices(j) - prices(i))
      }
    }
    profit
  }

  def maxProfitOpt(prices: Array[Int]): Int = {
    val n = prices.size
    var minPrice = Int.MaxValue
    var profit = 0
    for (i <- 0 until n) {
      if (prices(i) < minPrice) {
        minPrice = prices(i)
      } else if (prices(i) - minPrice > profit) {
        profit = prices(i) - minPrice
      }
    }
    profit
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(7, 1, 5, 3, 6, 4)
    println(maxProfitOpt(nums))
  }

}
