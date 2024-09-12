package com.learning.leetcode.top100.array

object MaxSubArray {
  /**
   * 最大连续子数组的和
   * 给你一个整数数组 nums ，请你找出一个具有最大和的连续子数组（子数组最少包含一个元素），返回其最大和。
   *
   * 子数组
   * 是数组中的一个连续部分。
   *
   *
   *
   * 示例 1：
   *
   * 输入：nums = [-2,1,-3,4,-1,2,1,-5,4]
   * 输出：6
   * 解释：连续子数组 [4,-1,2,1] 的和最大，为 6 。
   * 示例 2：
   *
   * 输入：nums = [1]
   * 输出：1
   * 示例 3：
   *
   * 输入：nums = [5,4,-1,7,8]
   * 输出：23
   *
   */

  /**
   * 思路：dp[i] = max{dp[i - 1] + nums(i), nums(i)}
   */
  def maxSubArray(nums: Array[Int]): Int = {
    var res = Int.MinValue
    var lastMax = 0
    for (num <- nums) {
      lastMax = Math.max(lastMax + num, num)
      res = Math.max(res, lastMax)
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(5,4,-1,7,8)
    println(maxSubArray(nums))
  }

}
