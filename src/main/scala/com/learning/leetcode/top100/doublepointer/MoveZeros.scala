package com.learning.leetcode.top100.doublepointer

object MoveZeros {

  /**
   * https://leetcode.cn/problems/move-zeroes/description/?envType=study-plan-v2&envId=top-100-liked
   * 给定一个数组 nums，编写一个函数将所有 0 移动到数组的末尾，同时保持非零元素的相对顺序。
   *
   * 请注意 ，必须在不复制数组的情况下原地对数组进行操作。
   *
   *
   *
   * 示例 1:
   *
   * 输入: nums = [0,1,0,3,12]
   * 输出: [1,3,12,0,0]
   * 示例 2:
   *
   * 输入: nums = [0]
   * 输出: [0]
   *
   *
   * 提示:
   *
   * 1 <= nums.length <= 104
   * -231 <= nums[i] <= 231 - 1
   *
   *
   * 进阶：你能尽量减少完成的操作次数吗？
   */

  def moveZeroes(nums: Array[Int]): Unit = {
    def _swap(i: Int, j: Int): Unit = {
      val t = nums(i)
      nums(i) = nums(j)
      nums(j) = t
    }
    if (nums.length > 1) {
      var i = 0
      var j = i + 1
      while (j < nums.length) {
        if (nums(i) == 0 && nums(j) != 0) {
          _swap(i, j)
          i = i + 1
          j = j + 1
        } else if (nums(i) == 0 && nums(j) == 0) {
          j = j + 1
        } else {
          i = i + 1
          j = j + 1
        }
      }
    }
  }

  def info(arr: Array[Int]): Unit = {
    println(arr.mkString("[", ",", "]"))
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(0,1,0,3,12)
    moveZeroes(nums)
    info(nums)

    val nums2 = Array(1, 0, 7, 0, 3, 12)
    moveZeroes(nums2)
    info(nums2)
  }

}
