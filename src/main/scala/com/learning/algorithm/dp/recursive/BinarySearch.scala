package com.learning.algorithm.dp.recursive

object BinarySearch {

  def doSearch(arr: Array[Int], target: Int, start: Int, end: Int): Int = {
    if (start < end) {
      val mid = (start + end) / 2
      if (arr(mid) == target) {
        mid
      } else if (arr(mid) < target) {
        doSearch(arr, target, mid + 1, end)
      } else {
        doSearch(arr, target, start, mid - 1)
      }
    } else {
      -1
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(2, 3, 6, 7, 9, 9, 11, 12)
    println(doSearch(arr, 9, 0, arr.length - 1))
    println(doSearch(arr, 3, 0, arr.length - 1))
    println(doSearch(arr, 13, 0, arr.length - 1))
  }

}
