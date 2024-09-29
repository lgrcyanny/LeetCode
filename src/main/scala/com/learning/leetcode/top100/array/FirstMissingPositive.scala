package com.learning.leetcode.top100.array

object FirstMissingPositive {

  /**
   * https://leetcode.cn/problems/first-missing-positive/description/?envType=study-plan-v2&envId=top-100-liked
   * 找到数组中没有出现的最小正正数
   * 原地hash
   * 数组长度为n，第一个缺失的正数要出现在[1, n]中, 如果都出现了是n+1
   * 1.遍历数组，所有的负数和0赋值为n+1
   * 2.所有<=n的数，对应的下标的位置变为负数，原地hash
   * 3.找到[1, n]中第一个正数的下标+1, 找不到就是n+1
   */
  def firstMissingPositive(nums: Array[Int]): Int = {
    val n = nums.length
    for (i <- 0 until n) {
      if (nums(i) <= 0) {
        nums(i) = n + 1
      }
    }
    for (i <- 0 until n) {
      val num = Math.abs(nums(i))
      if (num <= n) {
        nums(num - 1) = -Math.abs(nums(num - 1))
      }
    }
    var res = n + 1
    var continue = true
    for (i <- 0 until n if continue) {
      if (nums(i) > 0) {
        res = i + 1
        continue = false
      }
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(0)
    println(firstMissingPositive(nums))
  }

}
