package com.learning.algorithm.sort.v2.selection

object KthLargest {

  def swap(arr: Array[Int], m: Int, n: Int): Unit = {
    val t = arr(m)
    arr(m) = arr(n)
    arr(n) = t
  }

  def findKthLargest(nums: Array[Int], k: Int): Int = {
    for (i <- 0 until k) {
      var maxIndex = i
      for (j <- i + 1 until nums.length) {
        if (nums(j) > nums(maxIndex)) {
          maxIndex = j
        }
      }
      swap(nums, i, maxIndex)
    }
    nums(k - 1)
  }

  def main(args: Array[String]): Unit = {
    println(findKthLargest(Array(3, 2, 1, 5, 6, 4), 2))
  }

}
