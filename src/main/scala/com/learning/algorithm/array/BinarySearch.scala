package com.learning.algorithm.array

object BinarySearch {

  def search(nums: Array[Int], target: Int): Int = {
    var l = 0
    var r = nums.length - 1
    var index = -1
    while (l <= r && index < 0) {
      val mid = (l + r) / 2
      if (nums(mid) < target) {
        l = mid + 1
      } else if (nums(mid) > target) {
        r = mid - 1
      } else {
        index = mid
      }
    }
    index
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(-1, 0, 3, 5, 9, 12)
    val target = 9
    println(search(nums, 9))
    println(search(nums, -2))
  }

}
