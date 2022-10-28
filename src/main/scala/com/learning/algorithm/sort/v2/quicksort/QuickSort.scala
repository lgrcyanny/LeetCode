package com.learning.algorithm.sort.v2.quicksort

object QuickSort {

  def sort(nums: Array[Int]): Unit = {
    quickSort(nums, 0, nums.length - 1)
  }

  def quickSort(nums: Array[Int], start: Int, end: Int): Unit = {
    if (start < end) {
      val middle = partition(nums, start, end)
      quickSort(nums, start, middle - 1)
      quickSort(nums, middle + 1, end)
    }
  }

  def partition(nums: Array[Int], start: Int, end: Int): Int = {
    val pivot = nums(start)
    var left = start + 1
    var right = end
    while (left < right) {
      while(left < right && nums(left) <= pivot) {
        left = left + 1
      }
      if (left != right) {
        swap(nums, left, right)
        right = right - 1
      }
    }
    if (left == right && nums(right) > pivot) {
      right = right - 1
    }
    if (right != start) {
      swap(nums, right, start)
    }
    right
  }

  def swap(nums: Array[Int], m: Int, n: Int): Unit = {
    val t = nums(m)
    nums(m) = nums(n)
    nums(n) = t
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 3, 4, 6, 7, 9, 0)
    sort(nums)
    println(nums.mkString(", "))
  }

}
