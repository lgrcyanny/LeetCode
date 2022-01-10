package com.learning.algorithm.graph.shortestpath

import scala.collection.mutable

object BFS {

  /**
   *
   * @param graph unweighted graph
   * @param source
   * @param target
   * @return List of min path
   */
  def minPathSourceTarget(graph: Array[Array[Int]], source: Int, target: Int): List[Int] = {
    val queue = new mutable.Queue[List[Int]]()
    queue.enqueue(source :: Nil)
    var minPath: List[Int] = Nil
    var isDone = false
    while (queue.nonEmpty && !isDone) {
      val path = queue.dequeue()
      val lastNode = path.last
      if (lastNode == target) {
        isDone = true
        minPath = path
      }
      for (next <- graph(lastNode)) {
        queue.enqueue(path :+ next)
      }
    }
    minPath
  }

  def main(args: Array[String]): Unit = {
    val graph = Array(Array(1, 2), Array(2), Array(3), Array[Int]())
    println(minPathSourceTarget(graph, 0, 3).mkString(", "))
  }

}
