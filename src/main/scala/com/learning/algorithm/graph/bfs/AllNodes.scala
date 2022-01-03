package com.learning.algorithm.graph.bfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AllNodes {

  def allNodes(graph: Array[Array[Int]]): List[Int] = {
    val n = graph.size
    val source = 0
    val queue = new mutable.Queue[Int]()
    queue.enqueue(source)
    val visited = Array.ofDim[Boolean](n)
    val nodes = new ArrayBuffer[Int]()
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      if (!visited(node)) {
        visited(node) = true
        nodes.append(node)
      }
      for (next <- graph(node)) {
        queue.enqueue(next)
      }
    }
    nodes.toList
  }

  def main(args: Array[String]): Unit = {
    val graph = Array(
      Array(4, 3, 1),
      Array(3, 2, 4),
      Array(3),
      Array(4),
      Array[Int]()
    )
    val nodes = allNodes(graph)
    println(nodes.mkString(", "))
  }

}
