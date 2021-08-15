package com.learning.algorithm

object TilesPlacedInPlot {

  def countWays(n: Int): Int = {
    if (n == 0) {
      0
    } else if (n == 1) {
      1
    } else if (n == 2) {
      2
    } else {
      countWays(n - 1) + countWays(n - 2)
    }
  }

  def main(args: Array[String]): Unit = {
    println(countWays(5))
  }

}
