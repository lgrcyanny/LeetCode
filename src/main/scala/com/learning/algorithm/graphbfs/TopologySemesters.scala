package com.learning.algorithm.graphbfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object TopologySemesters {

  def minimumSemesters(n: Int, relations: Array[Array[Int]]): Int = {
    // 1.build graph
    val graph = new ArrayBuffer[ArrayBuffer[Int]]()
    for (i <- 0 until n) {
      graph.append(new ArrayBuffer[Int]())
    }
    val inDegree = Array.ofDim[Int](n)
    for (edge <- relations) {
      val source = edge(0) - 1
      val target = edge(1) - 1
      graph(source).append(target)
      inDegree(target) = inDegree(target) + 1
    }
    // 2.topo order
    var step = 0
    val queue = new mutable.Queue[Int]()
    for (i <- 0 until n if inDegree(i) == 0) {
      queue.enqueue(i)
    }
    while (queue.nonEmpty) {
      val size = queue.size
      for (i <- 0 until size) {
        val front = queue.dequeue()
        for (next <- graph(front)) {
          inDegree(next) = inDegree(next) - 1
          if (inDegree(next) == 0) {
            queue.enqueue(next)
          }
        }
      }
      step = step + 1
    }
    val hasLoop = inDegree.find(_ > 0).isDefined
    if (hasLoop) {
      -1
    } else {
      step
    }
  }

  def main(args: Array[String]): Unit = {
    val relations = Array(Array(1, 3), Array(2, 3))
    println(minimumSemesters(3, relations))
  }

}
