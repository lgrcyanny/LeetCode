package com.learning.algorithm.graph.bfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object AllPaths {

  def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] = {
    val n = graph.size
    val source = 0
    val target = n - 1
    val queue = new mutable.Queue[List[Int]]()
    queue.enqueue(source :: Nil)
    val paths = new ArrayBuffer[List[Int]]()
    while (queue.nonEmpty) {
      val path = queue.dequeue()
      val lastNode = path.last
      if (lastNode == target) {
        paths.append(path)
      }
      for (next <- graph(lastNode)) {
        queue.enqueue(path :+ next)
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
    val paths = allPathsSourceTarget(graph)
    paths.foreach(p => println(p.mkString(", ")))
  }

}
