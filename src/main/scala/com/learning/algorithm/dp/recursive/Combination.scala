package com.learning.algorithm.dp.recursive

object Combination {

  def com(n: Int, m: Int): Int = {
    if (n == 0 || m == 0 || m == n) {
      1
    } else {
      com(n - 1, m) + com(n-1, m - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    println(com(5, 4))
  }

}
