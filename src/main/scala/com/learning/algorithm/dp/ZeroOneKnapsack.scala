package com.learning.algorithm.dp

object ZeroOneKnapsack {
  val weight = Array(2, 3, 4, 5)
  val values = Array(3, 4, 5, 6)

  def maxValue(capacity: Int, n: Int): Int = {
    if (capacity <= 0 || n <= 0) {
      0
    } else if (weight(n - 1) > capacity) {
      maxValue(capacity, n - 1)
    } else {
      // nth item included
      val x = maxValue(capacity - weight(n - 1), n - 1) + values(n - 1)
      // nth item not included
      val y = maxValue(capacity, n - 1)
      val res = Math.max(x, y)
      res
    }
  }

  def maxValueDP(capacity: Int, n: Int): Int = {
    // memo(i, j) is max value that thief can carry if first i items are in the shop and capacity of knapsack is j
    val memo = Array.ofDim[Int](n + 1, capacity + 1)
    for (i <- 0 to n) {
      memo(i)(0) = 0
    }
    for (j <- 0 to capacity) {
      memo(0)(j) = 0
    }
    for (i <- 1 to n) {
      for (cp <- 1 to capacity) {
        if (weight(i - 1) <= cp) {
          val x = memo(i - 1)(cp - weight(i - 1)) + values(i - 1)
          val y = memo(i - 1)(cp)
          memo(i)(cp) = Math.max(x, y)
        } else {
          memo(i)(cp) = memo(i - 1)(cp)
        }
      }
    }
    println("memo is: ")
    for (i <- 0 to n) {
      println(memo(i).mkString(", "))
    }
    memo(n)(capacity)
  }

  def main(args: Array[String]): Unit = {
    val itemsCount = values.length
    println(maxValue(5, itemsCount))
    println(maxValueDP(capacity = 5, itemsCount))
  }

}
