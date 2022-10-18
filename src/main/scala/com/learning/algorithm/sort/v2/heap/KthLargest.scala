package com.learning.algorithm.sort.v2.heap

object KthLargest {

  def findKthLargest(nums: Array[Int], k: Int): Int = {
    buildMaxHeap(nums)
    // adjust k - 1 times
    for (i <- nums.length - 1 until nums.length - k by - 1) {
      swap(nums, 0, i)
      maxHeapify(nums, 0, i)
    }
    nums(0)
  }

  def buildMaxHeap(nums: Array[Int]): Unit = {
    val startIndex = nums.length / 2 - 1
    for (i <- startIndex to 0 by -1) {
      maxHeapify(nums, i, nums.length)
    }
  }

  def maxHeapify(nums: Array[Int], i: Int, heapSize: Int): Unit = {
    val left = 2 * i + 1
    val right = left + 1
    var largest = i
    if (left < heapSize && nums(left) > nums(largest)) {
      largest = left
    }
    if (right < heapSize && nums(right) > nums(largest)) {
      largest = right
    }
    if (largest != i) {
      swap(nums, largest, i)
      maxHeapify(nums, largest, heapSize)
    }
  }

  def swap(nums: Array[Int], m: Int, n: Int): Unit = {
    val t = nums(m)
    nums(m) = nums(n)
    nums(n) = t
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(3,2,3,1,2,4,5,5,6)
    val k = 4
    println(findKthLargest(nums, k))
  }

}
