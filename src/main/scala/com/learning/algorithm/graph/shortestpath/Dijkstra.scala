package com.learning.algorithm.graph.shortestpath

object Dijkstra {
  import scala.collection.mutable.ArrayBuffer
  import scala.collection.mutable

  /**
   * The distance from source to node
   */
  case class Dist(node: Int = -1, weight: Int = Int.MaxValue)
  implicit object DistOrdering extends Ordering[Dist] {
    override def compare(x: Dist, y: Dist): Int = y.weight compare x.weight
  }

  def networkDelayTime(times: Array[Array[Int]], n: Int, k: Int): Int = {
    // 1. build graph
    val graph = new ArrayBuffer[ArrayBuffer[(Int, Int)]]()
    for (i <- 0 until n) {
      graph.append(new ArrayBuffer[(Int, Int)]())
    }
    for (t <- times) {
      graph(t(0) - 1).append((t(1) - 1, t(2)))
    }
    // 2.Dijkstra algorithm based on min Heap
    val dists = new mutable.PriorityQueue[Dist]()
    val visited = Array.ofDim[Boolean](n)
    val minDist = Array.ofDim[Int](n)
    for (i <- 0 until n) {
      minDist(i) = Int.MaxValue
    }
    minDist(k - 1) = 0
    dists.enqueue(Dist(k - 1, 0))
    while (dists.nonEmpty) {
      val dist = dists.dequeue()
      visited(dist.node) = true
      minDist(dist.node) = Math.min(minDist(dist.node), dist.weight)
      for ((nextNode, weight) <- graph(dist.node) if !visited(nextNode)) {
        dists.enqueue(Dist(nextNode, dist.weight + weight))
      }
    }
    val maxTime = minDist.max
    if (maxTime == Int.MaxValue) -1 else maxTime
  }

  def main(args: Array[String]): Unit = {
    val times = Array(Array(2, 1, 1), Array(2, 3, 1), Array(3, 4, 1))
    val n = 4
    val k = 2
    println(networkDelayTime(times, n, k))
  }

}
