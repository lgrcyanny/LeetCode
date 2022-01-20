package com.learning.algorithm.graphdfs.app

import scala.collection.mutable.ArrayBuffer

object CountComponents {
  def countComponents(n: Int, edges: Array[Array[Int]]): Int = {
    // 1.build graph
    val graph = new ArrayBuffer[ArrayBuffer[Int]]()
    for (i <- 0 until n) {
      graph.append(new ArrayBuffer[Int]())
    }
    for (edge <- edges) {
      graph(edge(0)).append(edge(1))
      graph(edge(1)).append(edge(0))
    }
    // 2.run dfs
    val visited = Array.ofDim[Boolean](n)
    def _dfs(node: Int): Unit = {
      visited(node) = true
      for (next <- graph(node) if !visited(next)) {
        _dfs(next)
      }
    }
    var count = 0
    for (i <- 0 until n if !visited(i)) {
      _dfs(i)
      count = count + 1
    }
    count
  }



  def main(args: Array[String]): Unit = {
    val n = 2
    val edges = Array(Array(0, 1))
    println(countComponents(n, edges))
  }
}
