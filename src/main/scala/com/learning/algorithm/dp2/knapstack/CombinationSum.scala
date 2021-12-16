package com.learning.algorithm.dp2.knapstack

object CombinationSum {

  def combinationSum4(nums: Array[Int], target: Int): Int = {
    val n = nums.size
    val dp = Array.ofDim[Int](target + 1)
    dp(0) = 1
    for (j <- 1 to target) {
      for (i <- 0 until n) {
        if (j >= nums(i)) {
          dp(j) = dp(j) + dp(j - nums(i))
        }
      }
    }
    dp(target)
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 2, 3)
    println(combinationSum4(nums, 4))
  }

}
