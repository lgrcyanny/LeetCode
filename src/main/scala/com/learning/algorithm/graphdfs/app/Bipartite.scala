package com.learning.algorithm.graphdfs.app

object Bipartite {

  def isBipartite(graph: Array[Array[Int]]): Boolean = {
    val n = graph.size
    val visited = Array.ofDim[Boolean](n)
    val colors = Array.ofDim[Int](n)

    // return true if color successful
    def _dfs(u: Int, color: Int): Boolean = {
      visited(u) = true
      colors(u) = color
      var colorSuccessful = true
      for (next <- graph(u) if colorSuccessful) {
        if (!visited(next)) {
          if (!_dfs(next, 1 - color)) {
            colorSuccessful = false
          }
        } else if (colors(u) == colors(next)) {
          colorSuccessful = false
        }
      }
      colorSuccessful
    }

    var bipartite = true
    for (i <- 0 until n if bipartite && !visited(i)) {
      if (!_dfs(i, 0)) {
        bipartite = false
      }
    }
    bipartite
  }

  def main(args: Array[String]): Unit = {
    //    val graph = Array(Array(1, 2, 3), Array(0, 2), Array(0, 1, 3), Array(0, 2)) // false
    val graph = Array(Array(1, 3), Array(0, 2), Array(1, 3), Array(0, 2))
    println(isBipartite(graph))
  }

}
