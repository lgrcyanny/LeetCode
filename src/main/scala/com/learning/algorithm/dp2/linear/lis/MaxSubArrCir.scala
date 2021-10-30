package com.learning.algorithm.dp2.linear.lis

object MaxSubArrCir {

  /**
   * dp[i] means max sum from 0 to i
   * dp[i] = max(dp[i - 1] + nums(i), nums(i))
   */
  def maxSubArraySumCircular(nums: Array[Int]): Int = {
    val n = nums.size
    if (n == 1) {
      nums(0)
    } else {
      var maxSum = nums.max
      var minSum = nums.min
      var maxEnding = 0
      var minEnding = 0
      for (i <- 0 until n ) {
        maxEnding = Math.max(maxEnding + nums(i), nums(i))
        minEnding = Math.min(minEnding + nums(i), nums(i))
        maxSum = Math.max(maxSum, maxEnding)
        minSum = Math.min(minSum, minEnding)
      }
      val allSum = nums.sum
      if (maxSum < 0) {
        maxSum
      } else {
        Math.max(maxSum, allSum - minSum)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(-2, -3, -12)
    println(maxSubArraySumCircular(nums))
  }

}
