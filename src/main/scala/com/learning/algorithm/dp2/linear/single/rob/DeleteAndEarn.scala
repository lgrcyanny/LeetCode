package com.learning.algorithm.dp2.linear.single.rob

object DeleteAndEarn {

  def maxSubSum(nums: Array[Int]): Int = {
    val n = nums.size
    if (n == 0) {
      0
    } else if (n == 1) {
      nums(0)
    } else {
      var first = nums(0)
      var second = Math.max(first, nums(1))
      for (i <- 2 until n) {
        val t = second
        second = Math.max(second, first + nums(i))
        first = t
      }
      second
    }
  }

  def deleteAndEarn(nums: Array[Int]): Int = {
    val n = nums.size
    if (n == 0) {
      0
    } else if (n == 1) {
      nums(0)
    } else {
      val m = nums.max + 1
      val sum = Array.ofDim[Int](m)
      for (i <- 0 until m) {
        sum(i) = 0
      }
      for (v <- nums) {
        sum(v) = sum(v) + v
      }
      maxSubSum(sum)
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(2, 2, 3, 3, 3, 4)
    println(deleteAndEarn(nums))
  }

}
