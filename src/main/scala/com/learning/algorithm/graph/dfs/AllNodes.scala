package com.learning.algorithm.graph.dfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.HashSet

object AllNodes {

  def allNodesRecur(graph: Array[Array[Int]], source: Int): List[Int] = {
    val n = graph.size
    val visited = Array.ofDim[Boolean](n)
    val nodes = new ArrayBuffer[Int]()
    def _dfs(node: Int): Unit = {
      if (!visited(node)) {
        visited(node) = true
        nodes.append(node)
      }
      for (next <- graph(node)) {
        _dfs(next)
      }
    }
    _dfs(source)
    nodes.toList
  }

  def allNodes(graph: Array[Array[Int]], source: Int): List[Int] = {
    val n = graph.size
    val visited = Array.ofDim[Boolean](n)
    val nodes = new ArrayBuffer[Int]()
    val stack = new mutable.ArrayStack[Int]()
    stack.push(source)
    while (stack.nonEmpty) {
      val node = stack.pop()
      if (!visited(node)) {
        visited(node) = true
        nodes.append(node)
      }
      for (next <- graph(node)) {
        stack.push(next)
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
    val nodes = allNodesRecur(graph, 0)
    println(nodes.mkString(", "))
  }

}
