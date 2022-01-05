package com.learning.algorithm.graph.span

import scala.collection.mutable

object Prim {

  case class Edge(point1: Int, point2: Int, cost: Int)

  implicit object EdgeOrdering extends Ordering[Edge] {
    override def compare(a:Edge, b:Edge): Int = b.cost - a.cost
  }

  def calcDistance(x1: Array[Int], x2: Array[Int]): Int = Math.abs(x1(0) - x2(0)) + Math.abs(x1(1) - x2(1))

  def minCostConnectPoints(points: Array[Array[Int]]): Int = {
    val n = points.size
    val visited = Array.ofDim[Boolean](n)
    val edges = new mutable.PriorityQueue[Edge]()
    for (i <- 1 until n) {
      edges.enqueue(Edge(0, i, calcDistance(points(0), points(i))))
    }
    var count = n - 1
    visited(0) = true
    var totalCost = 0
    while (count > 0 && edges.nonEmpty) {
      val edge = edges.dequeue()
      if (!visited(edge.point2)) {
        visited(edge.point2) = true
        totalCost = totalCost + edge.cost
        for (j <- 0 until n) {
          if (!visited(j)) {
            edges.enqueue(Edge(edge.point2, j, calcDistance(points(edge.point2), points(j))))
          }
        }
        count = count - 1
      }
    }
    totalCost
  }

  def main(args: Array[String]): Unit = {
    val points = Array(Array(0, 0), Array(2, 2), Array(3, 10), Array(5, 2), Array(7, 0))
    println(minCostConnectPoints(points))
  }
}
