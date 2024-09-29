package com.learning.leetcode.top100.array

object ProductExceptSelf {
  /**
   * 给你一个整数数组 nums，返回 数组 answer ，其中 answer[i] 等于 nums 中除 nums[i] 之外其余各元素的乘积 。
   * 题目数据 保证 数组 nums之中任意元素的全部前缀元素和后缀的乘积都在  32 位 整数范围内。
   *
   * 请 不要使用除法，且在 O(n) 时间复杂度内完成此题。
   * 示例 1:
   *
   * 输入: nums = [1,2,3,4]  [1, 1, 2, 6]  [24, 12, 4, 1]
   * 输出: [24,12,8,6]
   * 示例 2:
   *
   * 输入: nums = [-1,1,0,-3,3]
   * 输出: [0,0,9,0,0]
   *
   */
  /**
   * 思路1: 两个数组, pre[i]前缀积，suf[i]后缀积，answer[i] = pre[i] * suf[i]
   */
  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    val length = nums.length
    val pre = Array.fill(length)(1)
    val suf = Array.fill(length)(1)
    for (i <- 1 until length) {
      pre(i) = pre(i - 1) * nums(i - 1)
    }
    for (i <- length - 2 to 0 by -1) {
      suf(i) = suf(i + 1) * nums(i + 1)
    }
    val ans = Array.fill(length)(1)
    for (i <- 0 until length) {
      ans(i) = pre(i) * suf(i)
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(10, 2)
    val res = productExceptSelf(nums)
    println(res.mkString(", "))
  }

}
