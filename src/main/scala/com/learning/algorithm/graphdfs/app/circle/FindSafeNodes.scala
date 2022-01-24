package com.learning.algorithm.graphdfs.app.circle
import scala.collection.mutable.ArrayBuffer

object FindSafeNodes {

  def eventualSafeNodes(graph: Array[Array[Int]]): List[Int] = {
    val NOT_VISIT = 0
    val VISITING = 1
    val SAFE = 2
    val n = graph.size
    val visited = Array.ofDim[Int](n)
    // return true if has circle
    def _dfs(u: Int): Boolean = {
      if (visited(u) != NOT_VISIT) {
        visited(u) == VISITING
      } else {
        visited(u) = VISITING
        var hasCircle = false
        for (next <- graph(u) if !hasCircle) {
          if (_dfs(next)) {
            hasCircle = true
          }
        }
        if (!hasCircle) {
          visited(u) = SAFE
        }
        hasCircle
      }
    }
    val res = new ArrayBuffer[Int]()
    for (i <- 0 until n) {
      if (!_dfs(i)) {
        res.append(i)
      }
    }
    res.toList
  }

  def main(args: Array[String]): Unit = {
    val graph = Array(Array(1, 2), Array(2, 3), Array(5), Array(0), Array(5), Array.empty[Int], Array.empty[Int])
    val nodes = eventualSafeNodes(graph)
    println(nodes.mkString(", "))
  }

}
