package com.learning.algorithm.graphdfs.app

import scala.collection.mutable.ArrayBuffer

object TopologyOrder {

  def findOrder(n: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    // build graph
    val graph = new ArrayBuffer[ArrayBuffer[Int]]()
    for (i <- 0 until n) {
      graph.append(new ArrayBuffer[Int]())
    }
    for (edge <- prerequisites) {
      val source = edge(1)
      val target = edge(0)
      graph(source).append(target)
    }
    // run dfs
    val res = new ArrayBuffer[Int]()
    val visited = Array.ofDim[Int](n)
    val NOT_VISIT = 0
    val VISITING = 1 // for loop detection
    val SAFE = 2 // means all successors finished
    // return true mean has loop
    def _dfs(u: Int): Boolean = {
      if (visited(u) == VISITING) {
        true
      } else if (visited(u) == SAFE) {
        false
      } else {
        visited(u) = VISITING
        var hasLoop = false
        for (next <- graph(u) if !hasLoop) {
          if (_dfs(next)) {
            hasLoop = true
          }
        }
        if (!hasLoop) {
          visited(u) = SAFE
          res.append(u)
        }
        hasLoop
      }
    }
    var hasLoop = false
    for (u <- 0 until n if !hasLoop && visited(u) == NOT_VISIT) {
      if (_dfs(u)) {
        hasLoop = true
        res.clear()
      }
    }
    res.reverse.toArray
  }

  def main(args: Array[String]): Unit = {
    val n = 4
    val prerequisites = Array(
      Array(1, 0), Array(2, 0), Array(3, 1), Array(3, 2)
    )
    println(findOrder(n, prerequisites).mkString(", "))
  }

}
