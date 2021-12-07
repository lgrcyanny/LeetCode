package com.learning.algorithm.dp2.prefixsum.hashmap

import scala.collection.mutable

object MaxArrLength {

  /**
   * sub array should contain equal number of 1 and 0
   * find max length of the sub array
   */
  def findMaxLength(nums: Array[Int]): Int = {
    val n = nums.size
    val map = new mutable.HashMap[Int, Int]()
    var count = 0
    var maxLen = 0
    map.put(0, -1)
    for (i <- 0 until n) {
      if (nums(i) == 0) {
        count = count - 1
      } else {
        count = count + 1
      }
      if (map.contains(count)) {
        maxLen = Math.max(maxLen, i - map(count))
      } else {
        map.put(count, i)
      }
    }
    maxLen
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(0, 0, 1, 0, 0, 0, 1, 1)
    println(findMaxLength(nums))
  }

}
