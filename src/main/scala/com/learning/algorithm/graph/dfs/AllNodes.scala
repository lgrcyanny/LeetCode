package com.learning.algorithm.graph.dfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.HashSet

object AllNodes {

  def allNodesRecur(graph: Array[Array[Int]]): List[Int] = {
    val n = graph.size
    val visited = Array.ofDim[Boolean](n)
    val nodes = new ArrayBuffer[Int]()
    def _dfs(node: Int): Unit = {
      visited(node) = true
      nodes.append(node)
      for (next <- graph(node) if !visited(next)) {
        _dfs(next)
      }
    }
    for (i <- 0 until n) {
      if (!visited(i)) {
        _dfs(i)
      }
    }
    nodes.toList
  }

  def allNodes(graph: Array[Array[Int]]): List[Int] = {
    val n = graph.size
    val visited = Array.ofDim[Boolean](n)
    val nodes = new ArrayBuffer[Int]()
    val stack = new mutable.ArrayStack[Int]()
    for (i <- 0 until n) {
      if (!visited(i)) {
        stack.push(i)
        while (stack.nonEmpty) {
          val node = stack.pop()
          nodes.append(node)
          visited(node) = true
          for (next <- graph(node) if !visited(next)) {
            stack.push(next)
          }
        }
      }
    }
    nodes.toList
  }

  def main(args: Array[String]): Unit = {
    val graph = Array(
      Array[Int](),
      Array(0, 3),
      Array(0),
      Array(2),
      Array(3)
    )
    val nodes = allNodes(graph)
    nodes.foreach(println)
  }

}
