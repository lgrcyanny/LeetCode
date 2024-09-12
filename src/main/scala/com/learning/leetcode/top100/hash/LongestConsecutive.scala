package com.learning.leetcode.top100.hash

import scala.collection.mutable

object LongestConsecutive {

  /**
   * https://leetcode.cn/problems/longest-consecutive-sequence/description/?envType=study-plan-v2&envId=top-100-liked
   * 给定一个未排序的整数数组 nums ，找出数字连续的最长序列（不要求序列元素在原数组中连续）的长度。
   *
   * 请你设计并实现时间复杂度为 O(n) 的算法解决此问题。
   *
   *
   *
   * 示例 1：
   *
   * 输入：nums = [100,4,200,1,3,2]
   * 输出：4
   * 解释：最长数字连续序列是 [1, 2, 3, 4]。它的长度为 4。
   * 示例 2：
   *
   * 输入：nums = [0,3,7,2,5,8,4,6,0,1]
   * 输出：9
   *
   * @param nums
   * @return
   */
  def longestConsecutive(nums: Array[Int]): Int = {
    if (nums.length <= 1) {
      nums.length
    } else {
      val dataSet = new mutable.HashSet[Int]()
      for (num <- nums) {
        dataSet.add(num)
      }
      def _doSearch(num: Int, length: Int): Int = {
        if (dataSet.contains(num + 1)) {
          _doSearch(num + 1, length + 1)
        } else {
          length
        }
      }
      var longestLen = 0
      for (num <- dataSet) {
        if (!dataSet.contains(num - 1)) {
          longestLen = Math.max(longestLen, _doSearch(num, 1))
        }
      }
      longestLen
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(100, 4, 200, 1, 3, 2)
    println(longestConsecutive(nums))

    val nums2 = Array(0,3,7,2,5,8,4,6,0,1)
    println(longestConsecutive(nums2))
  }

}
