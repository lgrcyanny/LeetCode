package com.learning.algorithm.sort.v2.couting

object CountingSort {

  def sort(nums: Array[Int]): Unit = {
    if (nums.length > 1) {
      val max = nums.max
      val min = nums.min
      val numRange = max - min + 1
      val counting = Array.ofDim[Int](numRange)
      for (element <- nums) {
        counting(element - min) = counting(element - min) + 1
      }
      var preCount = 0
      for (idx <- 0 until numRange) {
        preCount = preCount + counting(idx)
        counting(idx) = preCount - counting(idx)
      }
      val result = Array.ofDim[Int](nums.length)
      for (element <- nums) {
        result(counting(element - min)) = element
        counting(element - min) = counting(element - min) + 1
      }
      Array.copy(result, 0, nums, 0, result.length)
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 3, 1, 4, 5, 9, 7, 1, 2)
    sort(nums)
    println(nums.mkString(", "))
  }

}
