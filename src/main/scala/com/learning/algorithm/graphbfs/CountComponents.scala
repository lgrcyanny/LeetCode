package com.learning.algorithm.graphbfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CountComponents {

  def countComponents(n: Int, edges: Array[Array[Int]]): Int = {
    // 1.build graph
    val graph = new ArrayBuffer[ArrayBuffer[Int]]()
    for (i <- 0 until n) {
      graph.append(new ArrayBuffer[Int]())
    }
    for (edge <- edges) {
      val source = edge(0)
      val target = edge(1)
      graph(source).append(target)
      graph(target).append(source)
    }

    // run bfs
    var count = 0
    val visited = Array.ofDim[Boolean](n)
    for (i <- 0 until n) {
      if (!visited(i)) {
        bfs(i, graph, visited)
        count = count + 1
      }
    }
    count
  }

  def bfs(u: Int, graph: ArrayBuffer[ArrayBuffer[Int]], visited: Array[Boolean]): Unit = {
    val queue = new mutable.Queue[Int]()
    queue.enqueue(u)
    while (queue.nonEmpty) {
      val front = queue.dequeue()
      visited(front) = true
      for (next <- graph(front) if !visited(next)) {
        queue.enqueue(next)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val edges = Array(Array(0, 1), Array(1, 2), Array(3, 4))
    val count = countComponents(5, edges)
    println(count)
  }
}
