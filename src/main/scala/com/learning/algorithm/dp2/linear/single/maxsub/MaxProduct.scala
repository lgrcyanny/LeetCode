package com.learning.algorithm.dp2.linear.single.maxsub

object MaxProduct {

  /**
   * dp[i] means max value from 0 to i
   * maxdp[i] = max{maxdp[i - 1] * nums[i], mindp[i-1] * nums[i], nums(i)}
   * mindp[i] = min{mindp[i - 1] * nums[i], maxdp[i-1] * nums[i], nums(i)}
   */
  def maxProduct(nums: Array[Int]): Int = {
    def recur(i: Int): (Int, Int) = {
      if (i == 0) {
        (nums(0), nums(0))
      } else {
        val (maxPrev, minPrev) = recur(i - 1)
        val mx = Math.max(Math.max(maxPrev * nums(i), minPrev * nums(i)), nums(i))
        val mn = Math.min(Math.min(minPrev * nums(i), maxPrev * nums(i)), nums(i))
        (mx, mn)
      }
    }

    val res = (0 until nums.size).map { i =>
      val (mx, _) = recur(i)
      mx
    }
    res.max
  }

  def maxProductDP(nums: Array[Int]): Int = {
    var maxVal = nums(0)
    var minVal = nums(0)
    var res = nums(0)
    for (i <- 1 until nums.size) {
      val mx = Math.max(Math.max(maxVal * nums(i), minVal * nums(i)), nums(i))
      val mn = Math.min(Math.min(minVal * nums(i), maxVal * nums(i)), nums(i))
      maxVal = mx
      minVal = mn
      res = Math.max(res, mx)
    }
    res
  }

  def main(args: Array[String]): Unit = {
    //    val nums = Array(2, 3, -2, 4)
    //    val nums = Array(-3, -1, -1)
    //    val nums = Array(5, 6, -3, 4, -3)
    val nums = Array(-2, 0, -1)
    println(maxProduct(nums))
    println(maxProductDP(nums))
  }

}
