package com.learning.algorithm.dp2.knapstack

object CanEqualPartition {

  def canPartition(nums: Array[Int]): Boolean = {
    val n = nums.size
    val totalSum = nums.sum
    if (n == 1 || totalSum % 2 != 0) {
      false
    } else {
      val halfSum = totalSum / 2
      val dp = Array.ofDim[Int](halfSum + 1)
      dp(0) = 0
      for (j <- 1 to halfSum) {
        dp(j) = -1
      }
      for (i <- 0 until n) {
        for (j <- halfSum to 0 by -1) {
          if (j >= nums(i) && dp(j - nums(i)) != -1) {
            dp(j) = Math.max(dp(j), dp(j - nums(i)) + nums(i))
          }
        }
      }
      dp(halfSum) != -1
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 5, 11, 5)
    println(canPartition(nums))
  }

}
