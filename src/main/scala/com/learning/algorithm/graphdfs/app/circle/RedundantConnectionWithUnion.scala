package com.learning.algorithm.graphdfs.app.circle

object RedundantConnectionWithUnion {

  class UnionFind(n: Int) {
    val root = Array.ofDim[Int](n)
    val rank = Array.ofDim[Int](n)
    for (i <- 0 until n) {
      root(i) = i
      rank(i) = 1
    }

    def find(x: Int): Int = {
      if (x == root(x)) {
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
        } else if (rank(rootX) < rank(rootY)) {
          root(rootX) = rootY
        } else {
          root(rootY) = rootX
          rank(rootX) = rank(rootX) + 1
        }
      }
    }

    def isConnected(x: Int, y: Int): Boolean = find(x) == find(y)
  }

  def findRedundantConnection(edges: Array[Array[Int]]): Array[Int] = {
    val n = edges.flatten.max
    val uf = new UnionFind(n)
    var circleEdge = Array.empty[Int]
    for (edge <- edges if circleEdge.isEmpty) {
      val source = edge(0) - 1
      val target = edge(1) - 1
      if (uf.isConnected(source, target)) {
        circleEdge = edge
      } else {
        uf.union(source, target)
      }
    }
    circleEdge
  }

  def main(args: Array[String]): Unit = {
    val edges = Array(Array(1, 2), Array(2, 3), Array(2, 4), Array(4, 5), Array(1, 5))
    val res = findRedundantConnection(edges)
    println(res.mkString(", "))
  }

}
