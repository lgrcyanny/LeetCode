package com.learning.algorithm.dp2.linear.dimension

object LargestSumAvg {

  def calcAvg(prefix: Array[Int], i: Int, j: Int): Double = {
    (prefix(j + 1) - prefix(i)).toDouble / (j - i + 1)
  }

  def largestSumOfAverages(nums: Array[Int], groupCount: Int): Double = {
    val n = nums.size
    val prefix = Array.ofDim[Int](n + 1) // prefix sum from [0..i)
    prefix(0) = 0
    for (i <- 1 to n) {
      prefix(i) = prefix(i - 1) + nums(i - 1)
    }
    val dp = Array.ofDim[Double](n, groupCount + 1)
    for (i <- 0 until n) {
      dp(i)(0) = 0
      dp(i)(1) = calcAvg(prefix, 0, i)
    }
    for (i <- 0 until n) {
      for (k <- 2 to groupCount) {
        for (j <- 0 until i) {
          dp(i)(k) = Math.max(dp(i)(k), dp(j)(k - 1) + calcAvg(prefix, j + 1, i))
        }
      }
    }
    dp(n - 1)(groupCount)
  }

  def largestSumOfAveragesOpt(nums: Array[Int], groupCount: Int): Double = {
    val n = nums.size
    val prefix = Array.ofDim[Int](n + 1) // prefix sum from [0..i)
    prefix(0) = 0
    for (i <- 1 to n) {
      prefix(i) = prefix(i - 1) + nums(i - 1)
    }
    val dp = Array.ofDim[Double](n)
    for (i <- 0 until n) {
      dp(i) = calcAvg(prefix, 0, i)
    }
    for (k <- 2 to groupCount) {
      for (i <- n - 1 to 0 by -1) {
        for (j <- 0 until i) {
          dp(i) = Math.max(dp(i), dp(j) + calcAvg(prefix, j + 1, i))
        }
      }
    }
    dp(n - 1)
  }

  def main(args: Array[String]): Unit = {
//    val nums = Array(9, 1, 2, 3, 9) // expect 20
    val nums = Array(1,2,3,4,5,6,7) // expect 20.5
    println(largestSumOfAveragesOpt(nums, 4))
  }
}
