package com.learning.algorithm.dp2.linear.single.lis

object Envelopes {
  def maxEnvelopes(envelopes: Array[Array[Int]]): Int = {
    val n = envelopes.length
    // when width equal, should sort by height in descending order
    val sortedItems = envelopes.sortWith((a, b) => a(0) < b(0) || (a(0) == b(0) && a(1) >= b(1)))
    for (i <- 0 until n) {
      println(sortedItems(i).mkString(", "))
    }
    val lengths = Array.ofDim[Int](n)
    lengths(0) = 1
    for (i <- 1 until n) {
      lengths(i) = 1
      for (j <- 0 until i) {
        if (sortedItems(j)(0) < sortedItems(i)(0) && sortedItems(j)(1) < sortedItems(i)(1)) {
          if (lengths(j) + 1 > lengths(i)) {
            lengths(i) = lengths(j) + 1
          }
        }
      }
    }
    lengths.max
  }

  def main(args: Array[String]): Unit = {
    val envelopes = Array(Array(1, 5), Array(1, 4), Array(1, 2), Array(2, 3))
//    val envelopes = Array(Array(5, 4), Array(6, 4), Array(6, 7), Array(2, 3))
    println(maxEnvelopes(envelopes))
  }

}
