package com.learning.algorithm.sort.v2.bubble

import scala.collection.mutable.ArrayBuffer

object BubbleSort {

  def swap(arr: ArrayBuffer[Int], m: Int, n: Int): Unit = {
    val t = arr(m)
    arr(m) = arr(n)
    arr(n) = t
  }

  def swapOpt(arr: ArrayBuffer[Int], m: Int, n: Int): Unit = {
    arr(m) = arr(n) ^ arr(m)
    arr(n) = arr(n) ^ arr(m)
    arr(m) = arr(m) ^ arr(n)
  }

  def classicSort(arr: ArrayBuffer[Int]): Unit = {
    for (i <- 0 until arr.length) {
      for (j <- 0 until arr.length - i - 1) {
        if (arr(j) > arr(j + 1)) {
          swap(arr, j, j + 1)
        }
      }
    }
  }

  def optSort(arr: ArrayBuffer[Int]): Unit = {
    var swapped = true
    var lastUnsortedIndex = arr.length - 1
    while (swapped) {
      swapped = false
      var swapIndex = -1
      for (j <- 0 until lastUnsortedIndex) {
        if (arr(j) > arr(j + 1)) {
          swapOpt(arr, j, j + 1)
          swapped = true
          swapIndex = j
        }
      }
      lastUnsortedIndex = swapIndex
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = ArrayBuffer(6, 5, 1, 2, 3, 4)
    optSort(arr)
    println(arr.mkString(", "))
  }

}
