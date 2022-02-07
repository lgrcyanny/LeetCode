package com.learning.algorithm.graphbfs

import scala.collection.mutable

object NumSquares {
  def numSquares(n: Int): Int = {
    val queue = new mutable.Queue[Int]()
    queue.enqueue(n)
    val visited = Array.ofDim[Boolean](n + 1)
    visited(n) = true
    var step = 0
    var isDone = false
    while (queue.nonEmpty && !isDone) {
      val currentSize = queue.size
      for (i <- 0 until currentSize) {
        val front = queue.dequeue()
        for (j <- 1 to front if !isDone && j * j <= front) {
          if (j * j == front) {
            isDone = true
          } else {
            val next = front - j * j
            if (!visited(next)) {
              queue.enqueue(next)
              visited(next) = true
            }
          }
        }
      }
      step = step + 1
    }
    step
  }

  def numSquaresDP(n: Int): Int = {
    val dp = Array.ofDim[Int](n + 1)
    for (i <- 1 to n) {
      dp(i) = Int.MaxValue
    }
    dp(0) = 0
    for (i <- 1 to n) {
      for (j <- 1 to i if j * j <= i) {
        dp(i) = Math.min(dp(i), 1 + dp(i - j * j))
      }
    }
    dp(n)
  }

  def main(args: Array[String]): Unit = {
    val num = numSquaresDP(12)
    println(num)
  }

}
