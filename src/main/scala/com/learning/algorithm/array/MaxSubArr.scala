package com.learning.algorithm.array

object MaxSubArr {

  def maxSubArrDp(nums: Array[Int]): Int = {
    if (nums.length == 0) {
      0
    } else {
      var maxSum = nums(0)
      var maxEnding = 0
      for (num <- nums) {
        maxEnding = Math.max(num, num + maxEnding)
        if (maxEnding > maxSum) {
          maxSum = maxEnding
        }
      }
      maxSum
    }
  }

  def maxSubArrCumSum(nums: Array[Int]): Int = {
    if (nums.length == 0) {
      0
    } else if (nums.length == 1) {
      nums(0)
    } else {
      val cumSum = Array.ofDim[Int](nums.length)
      val minSum = Array.ofDim[Int](nums.length)
      cumSum(0) = nums(0)
      minSum(0) = nums(0)
      for (i <- 1 until nums.length) {
        cumSum(i) = cumSum(i - 1) + nums(i)
      }
      for (i <- 1 until nums.length) {
        minSum(i) = Math.min(cumSum(i), minSum(i - 1))
      }
      val diff = Array.ofDim[Int](nums.length)
      for (i <- 0 until nums.length) {
        diff(i) = cumSum(i) - minSum(i)
      }
      diff.max
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)
    val res = maxSubArrCumSum(nums)
    println(res)
  }

}
