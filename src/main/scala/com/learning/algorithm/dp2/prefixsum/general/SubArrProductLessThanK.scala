package com.learning.algorithm.dp2.prefixsum.general

object SubArrProductLessThanK {
  /**
   * It's not right on leetcode, but java version pass
   */
  def numSubarrayProductLessThanK(nums: Array[Int], k: Int): Int = {
    if (k == 0) {
      0
    } else {
      val n = nums.size
      val prefix = Array.ofDim[Double](n + 1)
      val logk = Math.log(k)
      prefix(0) = 0
      for (i <- 0 until n) {
        prefix(i + 1) = prefix(i) + Math.log(nums(i))
      }
      var count = 0
      for (i <- 0 until n) {
        var left = i + 1
        var right = n
        while (left < right) {
          val mid = (left + right) / 2
          if (prefix(mid) < prefix(i) + logk) {
            left = mid + 1
          } else {
            right = mid
          }
        }
        if (prefix(left) < prefix(i) + logk) {
          count = count + left - i + 1
        }
      }
      count - 1
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(10, 5, 2, 6)
    println(numSubarrayProductLessThanK(nums, 100))
  }
}
