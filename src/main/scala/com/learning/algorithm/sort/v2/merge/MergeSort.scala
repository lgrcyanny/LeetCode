package com.learning.algorithm.sort.v2.merge

object MergeSort {

  def sort(nums: Array[Int]): Unit = {
    mergeSort(nums, 0, nums.length - 1)
  }

  def mergeSort(nums: Array[Int], start: Int, end: Int): Unit = {
    if (start < end) {
      val mid = (start + end) / 2
      mergeSort(nums, start, mid)
      mergeSort(nums, mid + 1, end)
      merge(nums, start, end)
    }
  }

  def merge(nums: Array[Int], start: Int, end: Int): Unit = {
    if (start < end) {
      val mid = (start + end) / 2
      val start1 = start
      val end1 = mid
      val start2 = mid + 1
      val end2 = end
      val length = end - start + 1
      val result = Array.ofDim[Int](length)
      var index1 = start1
      var index2 = start2
      var resultIndex = 0
      while(index1 <= end1 && index2 <= end2) {
        if (nums(index1) <= nums(index2)) {
          result(resultIndex) = nums(index1)
          index1 = index1 + 1
        } else {
          result(resultIndex) = nums(index2)
          index2 = index2 + 1
        }
        resultIndex = resultIndex + 1
      }
      while (index1 <= end1) {
        result(resultIndex) = nums(index1)
        index1 = index1 + 1
        resultIndex = resultIndex + 1
      }
      while (index2 <= end2) {
        result(resultIndex) = nums(index2)
        index2 = index2 + 1
        resultIndex = resultIndex + 1
      }
      Array.copy(result, 0, nums, start, length)
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(6, 1, 5, 4, 9, 6, 7)
    sort(nums)
    println(nums.mkString(", "))
  }
}
