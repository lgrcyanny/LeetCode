package com.learning.algorithm.graph.topology

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Kahn {

  def findOrder(n: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    val graph = new ArrayBuffer[ArrayBuffer[Int]]()
    for (i <- 0 until n) {
      graph.append(new ArrayBuffer[Int]())
    }
    val indegree = Array.ofDim[Int](n)
    for (dep <- prerequisites) {
      val u = dep(1) // u -> v
      val v = dep(0)
      graph(u).append(v)
      indegree(v) = indegree(v) + 1
    }
    val queue = new mutable.Queue[Int]()
    for (i <- 0 until n if indegree(i) == 0) {
      queue.enqueue(i)
    }
    val order = new ArrayBuffer[Int]()
    while (queue.nonEmpty) {
      val u = queue.dequeue()
      order.append(u)
      for (nextNode <- graph(u)) {
        indegree(nextNode) = indegree(nextNode) - 1
        if (indegree(nextNode) == 0) {
          queue.enqueue(nextNode)
        }
      }
    }
    if (order.size < n) Array.empty else order.toArray
  }

  def main(args: Array[String]): Unit = {
    val n = 3
    val prerequisites = Array(
      Array(1, 0), Array(1, 2), Array(0, 1)
    )
    println(findOrder(n, prerequisites).mkString(", "))
  }


}
