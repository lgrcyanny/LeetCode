package com.learning.algorithm.graph.shortestpath

object BellmanFord {

  def networkDelayTime(times: Array[Array[Int]], n: Int, k: Int): Int = {
    val dist = Array.ofDim[Int](n)
    val INF = Int.MaxValue / 2
    for (i <- 0 until n) {
      dist(i) = INF
    }
    def _doRelax(u: Int, v: Int, weight: Int): Unit = {
      dist(v) = Math.min(dist(v), dist(u) + weight)
    }
    dist(k - 1) = 0
    for (i <- 0 until n) {
      for (t <- times) {
        val u = t(0) - 1
        val v = t(1) - 1
        val w = t(2)
        _doRelax(u, v, w)
      }
    }
    println(dist.mkString(", "))
    val maxTime = dist.max
    if (maxTime >= INF) -1 else maxTime
  }

  def main(args: Array[String]): Unit = {
    val times = Array(Array(2, 1, 1), Array(2, 3, 1), Array(3, 4, 1))
    val n = 4
    val k = 2
    println(networkDelayTime(times, n, k))
  }

}
