package com.learning.leetcode

/**
  * Created by lgrcyanny on 16/11/3.
  */
object HelloWorld {

  implicit class ToString(a: Int) {
    def asString(): String = a.toString
  }

  def quickSort(arr: Array[Int]): Array[Int] = {
    if (arr.length <= 1) arr
    else {
      val pivot = arr(arr.length / 2)
      Array.concat(
        quickSort(arr filter (pivot >)),
        arr filter (pivot ==),
        quickSort(arr filter (pivot <))
      )
    }
  }

  def main(args: Array[String]): Unit = {
    val data = Array(3, 1, 2)
    val sortedData = quickSort(data)
    println(sortedData.mkString(", "))
  }

}
