package com.learning.algorithm.dp.recursive

object Fib {
  def fib(n: Int): Int = {
    if (n == 1 || n == 2) {
      1
    } else {
      fib(n - 1) + fib(n - 2)
    }
  }

  def fibWithMemo(n: Int): Int = {
    val memo = Array.ofDim[Int](n + 1)
    memo(1) = 1
    memo(2) = 1
    for (i <- 3 to n) {
      memo(i) = memo(i - 1) + memo(i - 2)
    }
    memo(n)
  }

  def fibDP(n: Int): Int = {
    if (n == 1 || n == 2) {
      1
    } else {
      var a = 1
      var b = 1
      var c = 0
      for (i <- 3 to n) {
        c = a + b
        a = b
        b = c
      }
      c
    }
  }

  def main(args: Array[String]): Unit = {
    println(fib(10))
    println(fibWithMemo(10))
  }
}
