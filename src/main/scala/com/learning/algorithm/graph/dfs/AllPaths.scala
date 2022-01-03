package com.learning.algorithm.graph.dfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AllPaths {

  /**
   * It's a DAG, hence no need of visited table
   */
  def allPathsSourceTargetRecur(graph: Array[Array[Int]]): List[List[Int]] = {
    val n = graph.size
    val source = 0
    val target = n - 1
    val paths = new ArrayBuffer[List[Int]]()
    val path = new ArrayBuffer[Int]()
    def _dfs(node: Int): Unit = {
      path.append(node)
      if (node == target) {
        paths.append(path.toList)
      } else {
        for (next <- graph(node)) {
          _dfs(next)
          path.remove(path.size - 1)
        }
      }
    }
    _dfs(source)
    paths.toList
  }

  def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] = {
    val n = graph.size
    val source = 0
    val target = n - 1
    val paths = new ArrayBuffer[List[Int]]()
    val stack = new mutable.ArrayStack[List[Int]]()
    stack.push(source :: Nil)
    while (stack.nonEmpty) {
      val path = stack.pop()
      val lastVertex = path.last
      if (lastVertex == target) {
        paths.append(path)
      } else {
        for (node <- graph(lastVertex)) {
          stack.push(path :+ node)
        }
      }
    }
    paths.toList
  }

  def main(args: Array[String]): Unit = {
    val graph = Array(
      Array(4, 3, 1),
      Array(3, 2, 4),
      Array(3),
      Array(4),
      Array[Int]()
    )
    val paths = allPathsSourceTargetRecur(graph)
    paths.foreach(p => println(p.mkString(", ")))
  }
}
