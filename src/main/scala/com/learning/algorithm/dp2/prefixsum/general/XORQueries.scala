package com.learning.algorithm.dp2.prefixsum.general

import scala.collection.mutable.ArrayBuffer

object XORQueries {

  def xorQueries(arr: Array[Int], queries: Array[Array[Int]]): Array[Int] = {
    val n = arr.size
    val ans = new ArrayBuffer[Int]()
    val pre = Array.ofDim[Int](n + 1)
    pre(0) = 0
    for (i <- 0 until n) {
      pre(i + 1) = pre(i) ^ arr(i)
    }
    for (query <- queries) {
      ans.append(pre(query(0)) ^ pre(query(1) + 1))
    }
    ans.toArray
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(1, 3, 4, 8)
    val queries = Array(Array(0, 1), Array(1, 2), Array(0, 3), Array(3, 3))
    val ans = xorQueries(arr, queries)
    println(ans.mkString(", "))
  }

}
