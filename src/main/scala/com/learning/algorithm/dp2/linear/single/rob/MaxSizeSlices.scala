package com.learning.algorithm.dp2.linear.single.rob

object MaxSizeSlices {

  def doChoose(slices: Array[Int]): Int = {
    val n = slices.size
    val m = (n + 1) / 3
    val dp = Array.ofDim[Int](n + 1, m + 1)
    for (i <- 1 to n) {
      for (j <- 1 to m) {
        if (i < 2) {
          dp(i)(j) = Math.max(dp(i - 1)(j), slices(i - 1))
        } else {
          dp(i)(j) = Math.max(dp(i - 1)(j), dp(i - 2)(j - 1) + slices(i - 1))
        }
      }
    }
    dp(n)(m)
  }

  def maxSizeSlices(slices: Array[Int]): Int = {
    val n = slices.size
    val chooseFirst = doChoose(slices.slice(0, n - 1))
    val chooseNoFirst = doChoose(slices.slice(1, n))
    Math.max(chooseFirst, chooseNoFirst)
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(4, 1, 2, 5, 8, 3, 1, 9, 7)
    println(maxSizeSlices(nums))
  }

}
