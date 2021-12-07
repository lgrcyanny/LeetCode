package com.learning.algorithm.dp2.prefixsum.difference

object IntervalInc {

  def getModifiedArray(length: Int, updates: Array[Array[Int]]): Array[Int] = {
    val diff = Array.ofDim[Int](length)
    for (update <- updates) {
      val i = update(0)
      val j = update(1)
      val value = update(2)
      diff(i) = diff(i) + value
      if (j + 1 < length) {
        diff(j + 1) = diff(j + 1) - value
      }
    }
    for (i <- 1 until length) {
      diff(i) = diff(i - 1) + diff(i)
    }
    diff
  }

  def main(args: Array[String]): Unit = {
    val length = 5
    val updates = Array(
      Array(1, 3, 2),
      Array(2, 4, 3),
      Array(0, 2, -2)
    )
    println(getModifiedArray(length, updates).mkString(", "))
  }


}
