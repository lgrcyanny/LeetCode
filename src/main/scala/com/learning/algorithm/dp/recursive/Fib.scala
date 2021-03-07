package com.learning.algorithm.dp.recursive

object Fib {
  def fib(n: Int): Int = {
    if (n == 1 || n == 2) {
      1
    } else {
      fib(n - 1) + fib(n - 2)
    }
  }

  def main(args: Array[String]): Unit = {
    println(fib(80))
  }
}
