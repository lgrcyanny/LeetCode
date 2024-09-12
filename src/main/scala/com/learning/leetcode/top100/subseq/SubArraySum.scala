package com.learning.leetcode.top100.subseq

import scala.collection.mutable

object SubArraySum {
  /**
   * 和为K的子数组
   * 给你一个整数数组 nums 和一个整数 k ，请你统计并返回 该数组中和为 k 的子数组的个数 。
   * 子数组是数组中元素的连续非空序列。
   * 示例 1：
   *
   * 输入：nums = [1,1,1], k = 2
   * 输出：2
   * 示例 2：
   *
   * 输入：nums = [1,2,3], k = 3
   * 输出：2
   *
   */

  /**
   * 枚举法
   */
   def subarraySumV1(nums: Array[Int], k: Int): Int = {
     var ans = 0
     for (i <- 0 until nums.length) {
       var sum = 0
       for (j <- i until nums.length) {
         sum = sum + nums(j)
         if (sum == k) {
           ans = ans + 1
         }
       }
     }
     ans
   }

  /**
   * 前缀和+hash表
   * hash: [key, value], key是和，value是次数
   * @param args
   */
  def subarraySumV2(nums: Array[Int], k: Int): Int = {
    var ans = 0
    val mp = new mutable.HashMap[Int, Int]()
    var pre = 0
    mp.put(0, 1)
    for (i <- 0 until nums.length) {
      pre = nums(i) + pre
      if (mp.contains(pre - k)) {
        ans = ans + mp(pre - k)
      }
      mp.put(pre, mp.getOrElse(pre, 0) + 1)
    }
    ans
  }


  def main(args: Array[String]): Unit = {
    val nums = Array(1, 2, 3)
    println(subarraySumV2(nums, 3))
  }

}
