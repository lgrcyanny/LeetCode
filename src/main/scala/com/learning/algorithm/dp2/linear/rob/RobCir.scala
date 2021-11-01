package com.learning.algorithm.dp2.linear.rob

object RobCir {

  def robRange(nums: Array[Int], start: Int, end: Int): Int = {
    var first = nums(start)
    var second = Math.max(first, nums(start + 1))
    for (i <- start + 2 to end) {
      val temp = second
      second = Math.max(second, first + nums(i))
      first = temp
    }
    second
  }

  def rob(nums: Array[Int]): Int = {
    val n = nums.size
    if (n == 0) {
      0
    } else if (n == 1) {
      nums(0)
    } else if (n == 2) {
      nums.max
    } else {
      Math.max(robRange(nums, 0, n - 2), robRange(nums, 1, n - 1))
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(2, 3, 2)
    println(rob(nums))
  }

}
