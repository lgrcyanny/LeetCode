package com.learning.algorithm.graph.shortestpath
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object SPFA {

  def networkDelayTime(times: Array[Array[Int]], n: Int, k: Int): Int = {
    // 1. build graph
    val graph = new ArrayBuffer[ArrayBuffer[(Int, Int)]]()
    for (i <- 0 until n) {
      graph.append(new ArrayBuffer[(Int, Int)]())
    }
    for (t <- times) {
      graph(t(0) - 1).append((t(1) - 1, t(2)))
    }
    // 2. run spfa algorithm
    val dist = Array.ofDim[Int](n)
    val INF = Int.MaxValue / 2
    for (i <- 0 until n) {
      dist(i) = INF
    }
    val queue = new mutable.Queue[Int]()
    val isInQueue = Array.ofDim[Boolean](n)
    queue.enqueue(k - 1)
    isInQueue(k - 1) = true
    dist(k - 1) = 0
    while (queue.nonEmpty) {
      val u = queue.dequeue()
      isInQueue(u) = false
      for ((nextNode, weight) <- graph(u)) {
        if (dist(u) + weight < dist(nextNode)) {
          dist(nextNode) = dist(u) + weight // do relax
          queue.enqueue(nextNode)
          isInQueue(nextNode) = true
        }
      }
    }
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
