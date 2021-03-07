package com.learning.algorithm.dp.recursive

object BubbleSort {

  def swap(arr: Array[Int], i: Int, j: Int): Unit = {
    val t = arr(i)
    arr(i) = arr(j)
    arr(j) = t
  }

  def doSort(arr: Array[Int], n: Int): Unit = {
    if (n > 1) {
      for (i <- 0 until n - 1) {
        if (arr(i) > arr(i + 1)) swap(arr, i, i + 1)
      }
      doSort(arr, n - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(9, 6, 2, 12, 11, 9, 3, 7)
    doSort(arr, arr.length)
    println(arr.mkString(", "))
  }

}
