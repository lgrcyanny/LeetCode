package com.learning.leetcode.top100.array

object RotateArray {
  /**
   * 给定一个整数数组 nums，将数组中的元素向右轮转 k 个位置，其中 k 是非负数。
   * 示例 1:
   *
   * 输入: nums = [1,2,3,4,5,6,7], k = 3
   * 输出: [5,6,7,1,2,3,4]
   * 解释:
   * 向右轮转 1 步: [7,1,2,3,4,5,6]
   * 向右轮转 2 步: [6,7,1,2,3,4,5]
   * 向右轮转 3 步: [5,6,7,1,2,3,4]
   * 示例 2:
   *
   * 输入：nums = [-1,-100,3,99], k = 2
   * 输出：[3,99,-1,-100]
   * 解释:
   * 向右轮转 1 步: [99,-1,-100,3]
   * 向右轮转 2 步: [3,99,-1,-100]
   */
  /**
   * solution1: 需要一个临时数组，按轮转规则拷贝, new_num(i + k % n) = nums(i)
   * solution2: 环状替换
   * solution3: 数组翻转三次
   */
  def rotate(nums: Array[Int], k: Int): Unit = {
    def _reverse(start: Int, end: Int): Unit = {
      var i = start
      var j = end
      while (i < j) {
        val t = nums(i)
        nums(i) = nums(j)
        nums(j) = t
        i = i + 1
        j = j - 1
      }
    }
    // Warning: 注意K要取余，把长度修正，数组长度会小于K
    val rotateK = k % nums.length
    _reverse(0, nums.length - 1)
    _reverse(0, rotateK - 1)
    _reverse(rotateK, nums.length - 1)
    nums
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 2)
    rotate(nums, 3)
    println(nums.mkString(", "))
  }




}
