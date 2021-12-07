package com.learning.algorithm.dp2.prefixsum.hashmap

import scala.collection.mutable

object MaxSubArrayLen {

  def maxSubArrayLen(nums: Array[Int], k: Int): Int = {
    val n = nums.size
    val map = new mutable.HashMap[Int, Int]()
    var prefixSum = 0
    var maxLen = 0
    map.put(0, -1)
    for (i <- 0 until n) {
      prefixSum = prefixSum + nums(i)
      if (!map.contains(prefixSum)) {
        map.put(prefixSum, i)
      }
      maxLen = Math.max(maxLen, i - map.getOrElse(prefixSum - k, i))
    }
    maxLen
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, -1, 5, -2, 3)
    println(maxSubArrayLen(nums, 3))
  }

}
