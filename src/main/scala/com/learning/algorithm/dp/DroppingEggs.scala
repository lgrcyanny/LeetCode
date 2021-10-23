package com.learning.algorithm.dp

object DroppingEggs {

  def droppingEggs(numFloors: Int, numEggs: Int): Int = {
    if (numFloors == 1 || numFloors == 0 || numEggs == 1) {
      numFloors
    } else {
      var min = Int.MaxValue
      for (p <- 1 to numFloors) {
        val t = Math.max(droppingEggs(p - 1, numEggs - 1), droppingEggs(numFloors - p, numEggs))
        if (t < min) {
          min = t
        }
      }
      min + 1
    }
  }

  def droppingEggsDP(numFloors: Int, numEggs: Int): Int = {
    val memo = Array.ofDim[Int](numFloors + 1, numEggs + 1)
    for (j <- 0 to numEggs) {
      memo(0)(j) = 0
      memo(1)(j) = 1
    }
    for (i <- 2 to numFloors) {
      memo(i)(1) = i
    }
    for (numf <- 1 to numFloors) {
      for (e <- 1 to numEggs) {
        var min = Int.MaxValue
        for (p <- 1 to numf) {
          val t = Math.max(memo(p - 1)(e - 1), memo(numf - p)(e))
          if (t < min) {
            min = t
          }
        }
        memo(numf)(e) = min + 1
      }
    }
    memo(numFloors)(numEggs)
  }

  def main(args: Array[String]): Unit = {
    println(droppingEggsDP(100, 2))
  }

}
