package com.learning.algorithm.dp2.prefixsum.hashmap

import scala.collection.mutable

object SubArrayCount {

  def subarraySum(nums: Array[Int], k: Int): Int = {
    val n = nums.size
    val map = new mutable.HashMap[Int, Int]()
    map.put(0, 1)
    var prefixSum = 0
    var maxCount = 0
    for (i <- 0 until n) {
      prefixSum = prefixSum + nums(i)
      val t = prefixSum - k
      if (map.contains(t)) {
        maxCount = maxCount + map(t)
      }
      map.put(prefixSum, map.getOrElse(prefixSum, 0) + 1)
    }
    maxCount
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 2, 3)
    println(subarraySum(nums, 3))
  }

}
