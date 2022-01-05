package com.learning.algorithm.graph.span

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Kruskal {

  case class Edge(x: Int, y: Int, cost: Int)

  implicit object EdgeOrdering extends Ordering[Edge] {
    override def compare(a:Edge, b:Edge): Int = b.cost - a.cost
  }

  def calcDistance(x1: Array[Int], x2: Array[Int]): Int = Math.abs(x1(0) - x2(0)) + Math.abs(x1(1) - x2(1))

  class UnionFind(size: Int) {
    val root = Array.ofDim[Int](size)
    val rank = Array.ofDim[Int](size)
    for (i <- 0 until size) {
      root(i) = i
      rank(i) = 1
    }

    def find(x: Int): Int = {
      if (root(x) == x) {
        x
      } else {
        root(x) = find(root(x))
        root(x)
      }
    }

    def union(x: Int, y: Int): Unit = {
      val rootX = find(x)
      val rootY = find(y)
      if (rootX != rootY) {
        if (rank(rootX) > rank(rootY)) {
          root(rootY) = rootX
        } else if (rank(rootX) > rank(rootY)) {
          root(rootX) = rootY
        } else {
          root(rootY) = rootX
          rank(rootX) = rank(rootX) + 1
        }

      }
    }

    def connected(x: Int, y: Int): Boolean = find(x) == find(y)
  }


  /**
   * Kruskal algorithms
   * 1.generate all edges and add to priority queue
   * 2.take an edge from queue, verify if it already connected in UnionFind, if connected means has circle
   * 3.stop when add n - 1 edges to graph
   */
  def minCostConnectPoints(points: Array[Array[Int]]): Int = {
    val n = points.size
    val edges = new mutable.PriorityQueue[Edge]
    val uf = new UnionFind(n)
    for (i <- 0 until n) {
      for (j <- i + 1 until n) {
        edges.enqueue(Edge(i, j, calcDistance(points(i), points(j))))
      }
    }
    var count = n - 1
    var totalCost = 0
    while (count > 0 && edges.nonEmpty) {
      val edge = edges.dequeue()
      if (!uf.connected(edge.x, edge.y)) {
        uf.union(edge.x, edge.y)
        totalCost = totalCost + edge.cost
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
