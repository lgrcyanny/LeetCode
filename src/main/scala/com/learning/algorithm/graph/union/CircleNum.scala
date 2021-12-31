package com.learning.algorithm.graph.union

object CircleNum {

  class UnionFind(size: Int) {
    var connectedSize: Int = size
    val root = Array.ofDim[Int](size)
    val rank = Array.ofDim[Int](size)
    for (i <- 0 until size) {
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
        } else if (rank(rootX) < root(rootY)) {
          root(rootX) = rootY
        } else {
          root(rootY) = rootX
          rank(rootX) = rank(rootX) + 1
        }
        connectedSize = connectedSize - 1
      }
    }
  }

  def findCircleNum(isConnected: Array[Array[Int]]): Int = {
    val n = isConnected.size
    val uf = new UnionFind(n)
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        if (i != j && isConnected(i)(j) == 1) {
          uf.union(i, j)
        }
      }
    }
    uf.connectedSize
  }

  def findCircleNumDFS(isConnected: Array[Array[Int]]): Int = {
    val n = isConnected.size
    val visited = Array.ofDim[Boolean](n)
    def _dfs(i: Int): Unit = {
      visited(i) = true
      for (j <- 0 until n) {
        if (!visited(j) && isConnected(i)(j) == 1) {
          _dfs(j)
        }
      }
    }
    var count = 0
    for (i <- 0 until n) {
      if (!visited(i)) {
        _dfs(i)
        count = count + 1
      }
    }
    count
  }

  def main(args: Array[String]): Unit = {
    val isConnected = Array(
      Array(1, 0, 0, 1),
      Array(0, 1, 1, 0),
      Array(0, 1, 1, 1),
      Array(1, 0, 1, 1)
    )
    println(findCircleNum(isConnected))
  }

}
