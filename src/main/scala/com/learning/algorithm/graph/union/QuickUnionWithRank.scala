package com.learning.algorithm.graph.union

object QuickUnionWithRank {
  class UnionFind(size: Int) {
    val root = Array.ofDim[Int](size)
    val rank = Array.ofDim[Int](size)
    for (i <- 0 until size) {
      root(i) = i
      rank(i) = 1
    }

    def find(x: Int): Int = {
      var r = x
      while (r != root(r)) {
        r = root(r)
      }
      r
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

  def main(args: Array[String]): Unit = {
    val uf = new UnionFind(10)
    // 1-2-5-6-7 3-8-9 4
    uf.union(1, 2)
    uf.union(2, 5)
    uf.union(6, 7)
    uf.union(5, 6)
    uf.union(3, 8)
    uf.union(8, 9)
    println(uf.connected(1, 7)) // true
    println(uf.connected(1, 4)) // false
    println(uf.connected(5, 9)) // false
    // 1-2-5-6-7 3-8-9-4
    uf.union(9, 4)
    println(uf.connected(3, 4)) // true
  }
}
