package com.learning.algorithm.dp2.prefixsum.hashmap

import scala.collection.mutable

object SubArraySumByK {

  def checkSubarraySum(nums: Array[Int], k: Int): Boolean = {
    val n = nums.size
    var prefixSum = 0
    val map = new mutable.HashMap[Int, Int]()
    var i = 0
    var flag = false
    map.put(0, -1)
    while (i < n && !flag) {
      prefixSum = prefixSum + nums(i)
      val t = prefixSum % k
      if (map.contains(t)) {
        if (i - map(t) > 1) {
          flag = true
        }
      } else {
        map.put(t, i)
      }
      i = i + 1
    }
    flag
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(2, 5, 33, 6, 7, 25, 15)
    println(checkSubarraySum(nums, 13))
  }

}
