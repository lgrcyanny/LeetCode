package com.learning.algorithm.graphdfs.app.circle

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object RedundantConnection {

  def findRedundantConnection(edges: Array[Array[Int]]): Array[Int] = {
    val graph = new mutable.HashMap[Int, ArrayBuffer[Int]]()
    val visited = new mutable.HashSet[Int]()
    def _addEdge(u: Int, v: Int): Unit = {
      if (graph.contains(u)) {
        graph(u).append(v)
      } else {
        graph.put(u, new ArrayBuffer[Int]())
        graph(u).append(v)
      }
    }
    def _dfs(source: Int, target: Int): Boolean = {
      if (source == target) {
        true
      } else {
        visited.add(source)
        var hasCircle = false
        for (next <- graph(source) if !visited(next) && !hasCircle) {
          if (_dfs(next, target)) {
            hasCircle = true
          }
        }
        hasCircle
      }
    }
    var circleEdge = Array.empty[Int]
    for (edge <- edges if circleEdge.isEmpty) {
      val source = edge(0)
      val target = edge(1)
      if (graph.contains(source) && graph.contains(target)) {
        visited.clear()
        if (_dfs(source, target)) {
          circleEdge = edge
        }
      }
      _addEdge(source, target)
      _addEdge(target, source)
    }
    circleEdge
  }

  def main(args: Array[String]): Unit = {
    val edges = Array(Array(1, 2), Array(2, 3), Array(3, 4), Array(1, 4), Array(1, 5))
    val res = findRedundantConnection(edges)
    println(res.mkString(", "))
  }

}
