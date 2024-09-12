package com.learning.leetcode.top100.doublepointer

import scala.collection.mutable.ArrayBuffer

object ThreeSum {
  /**
   * 给你一个整数数组 nums ，判断是否存在三元组 [nums[i], nums[j], nums[k]] 满足 i != j、i != k 且 j != k ，同时还满足 nums[i] + nums[j] + nums[k] == 0 。请你返回所有和为 0 且不重复的三元组。
   *
   * 注意：答案中不可以包含重复的三元组。
   * 示例 1：
   *
   * 输入：nums = [-1,0,1,2,-1,-4]
   * 输出：[[-1,-1,2],[-1,0,1]]
   * 解释：
   * nums[0] + nums[1] + nums[2] = (-1) + 0 + 1 = 0 。
   * nums[1] + nums[2] + nums[4] = 0 + 1 + (-1) = 0 。
   * nums[0] + nums[3] + nums[4] = (-1) + 2 + (-1) = 0 。
   * 不同的三元组是 [-1,0,1] 和 [-1,-1,2] 。
   * 注意，输出的顺序和三元组的顺序并不重要。
   */
  def threeSum(input: Array[Int]): List[List[Int]] = {
    val nums = input.sorted
    val result = ArrayBuffer.empty[List[Int]]
    for (i <- 0 until nums.length) {
      if (i == 0 || nums(i) != nums(i - 1)) {
        var k = nums.length - 1
        for (j <- i + 1 until nums.length) {
          if (j < k && (j == i + 1 || nums(j) != nums(j - 1))) {
            while (j < k && nums(i) + nums(j) + nums(k) > 0) {
              k = k - 1
            }
            if (i < j && j < k && nums(i) + nums(j) + nums(k) == 0) {
              result.append(List(nums(i), nums(j), nums(k)))
            }
          }
        }
      }
    }
    result.toList
  }

  def info(result: List[List[Int]]): Unit = {
    println(result.mkString(", "))
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(0, 0, 0)
    println(nums.sorted.toList)
    info(threeSum(nums))
  }

}
