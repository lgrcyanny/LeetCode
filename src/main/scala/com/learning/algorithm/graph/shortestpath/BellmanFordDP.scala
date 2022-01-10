package com.learning.algorithm.graph.shortestpath

object BellmanFordDP {

  def findCheapestPrice(n: Int, flights: Array[Array[Int]], src: Int, dst: Int, K: Int): Int = {
    val INF = Int.MaxValue / 2
    val previous = Array.ofDim[Int](n)
    val current = Array.ofDim[Int](n)
    for (i <- 0 until n) {
      current(i) = INF
      previous(i) = INF
    }
    previous(src) = 0
    for (k <- 1 to K + 1) {
      for (edge <- flights) {
        val u = edge(0)
        val v = edge(1)
        val weight = edge(2)
        current(v) = Math.min(current(v), previous(u) + weight)
      }
      current.copyToArray(previous)
    }
    if (current(dst) >= INF) -1 else current(dst)
  }

  def main(args: Array[String]): Unit = {
    val flights = Array(
      Array(0, 1, 100),
      Array(1, 2, 100),
      Array(0, 2, 500))
    println(findCheapestPrice(n = 3, flights=flights, src = 0, dst = 2, K = 1))
  }

}
