package com.learning.algorithm.sort.v2.heap

object HeapSort {

  def sort(nums: Array[Int]): Unit = {
    buildMaxHeap(nums)
    for (i <- nums.length - 1 to 0  by - 1) {
      swap(nums, 0, i)
      maxHeapify(nums, 0, i)
    }
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
    val arr = Array(6, 4, 3, 2, 4, 1, 7)
    sort(arr)
    println(arr.mkString(", "))
  }

}
