package com.learning.algorithm.dp2.statecompression

import scala.collection.mutable

object GraphShortestPath {

  def shortestPathLengthBFS(graph: Array[Array[Int]]): Int = {
    val visited = new mutable.HashSet[(Int, Int)]()
    val queue = new mutable.Queue[(Int, Int, Int)]() // identity, mask, distance
    val n = graph.size
    for (i <- 0 until n) {
      visited.add((i, 1 <<i))
      queue.enqueue((i, 1 << i, 0))
    }
    var isDone = false
    var res = 0
    while (queue.nonEmpty && !isDone) {
      val (i, mask, dist) = queue.dequeue()
      if (mask == ((1 << n) - 1)) {
        isDone = true
        res = dist
      }
      for (j <- graph(i)) {
        val maskForJ = mask | 1 << j
        if (!visited.contains((j, maskForJ))) {
          queue.enqueue((j, maskForJ, dist + 1))
          visited.add((j, maskForJ))
        }
      }
    }
    res
  }

  def shortestPathLength(graph: Array[Array[Int]]): Int = {
    val n = graph.size
    val states = 1 << n
    val dp = Array.ofDim[Int](states, n)
    val distance = Array.ofDim[Int](n, n + 1)
    // floyd algorithm for min distance between each pair
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        distance(i)(j) = n + 1
        if (graph(i).contains(j)) {
          distance(i)(j) = 1
        }
      }
    }
    for (k <- 0 until n) {
      for (i <- 0 until n) {
        for (j <- 0 until n) {
          distance(i)(j) = Math.min(distance(i)(j), distance(i)(k) + distance(k)(j))
        }
      }
    }
    // dp algorithm
    for (s <- 0 until states) {
      for (i <- 0 until n) {
        dp(s)(i) = Int.MaxValue
      }
    }
    for (s <- 1 until states) {
      for (i <- 0 until n) {
        // s is 2 ^ k
        if ((s & (s - 1)) == 0) {
          dp(s)(i) = 0
        } else {
          if ((s & 1 << i) != 0) {
            for (v <- 0 until n) {
              if ((s & (1 << v)) != 0 && (v != i)) {
                dp(s)(i) = Math.min(dp(s)(i), dp(s ^ (1 << i))(v) + distance(v)(i))
              }
            }
          }
        }
      }
    }
    dp(states - 1).min
  }

  def main(args: Array[String]): Unit = {
    val graph = Array(Array(1), Array(0, 2, 4), Array(1, 3, 4), Array(2), Array(1, 2))
    println(shortestPathLengthBFS(graph))
  }

}
