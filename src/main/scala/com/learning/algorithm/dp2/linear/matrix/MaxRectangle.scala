package com.learning.algorithm.dp2.linear.matrix

import scala.collection.mutable

object MaxRectangle {

  def maximalRectangle(matrix: Array[Array[Char]]): Int = {
    val m = matrix.size
    if (m == 0) {
      0
    } else {
      val n = matrix(0).size
      val widthMatrix = Array.ofDim[Int](m, n)
      for (i <- 0 until m) {
        for (j <- 0 until n) {
          if (matrix(i)(j) == '1') {
            if (j == 0) {
              widthMatrix(i)(j) = 1
            } else {
              widthMatrix(i)(j) = widthMatrix(i)(j - 1) + 1
            }
          } else {
            widthMatrix(i)(j) = 0
          }
        }
      }
      var maxArea = 0
      for (i <- 0 until m) {
        for (j <- 0 until n) {
          if (matrix(i)(j) == '1') {
            var width = widthMatrix(i)(j)
            var area = width
            for (k <- i - 1 to 0 by -1) {
              width = Math.min(width, widthMatrix(k)(j))
              area = Math.max(area, (i - k + 1) * width)
            }
            maxArea = Math.max(area, maxArea)
          }
        }
      }
      maxArea
    }
  }

  def maximalRectangleOpt(matrix: Array[Array[Char]]): Int = {
    val m = matrix.size
    if (m == 0) {
      0
    } else {
      val n = matrix(0).size
      val widthMatrix = Array.ofDim[Int](m, n)
      for (i <- 0 until m) {
        for (j <- 0 until n) {
          if (matrix(i)(j) == '1') {
            if (j == 0) {
              widthMatrix(i)(j) = 1
            } else {
              widthMatrix(i)(j) = widthMatrix(i)(j - 1) + 1
            }
          } else {
            widthMatrix(i)(j) = 0
          }
        }
      }
      var maxArea = 0
      for (j <- 0 until n) {
        val up = Array.ofDim[Int](m) // search from down to up
        val down = Array.ofDim[Int](m) // search from up to down
        val stack = new mutable.ArrayStack[Int]()
        for (i <- 0 until m) {
          while (stack.nonEmpty && widthMatrix(stack.top)(j) >= widthMatrix(i)(j)) {
            stack.pop()
          }
          down(i) = if (stack.isEmpty) -1 else stack.top
          stack.push(i)
        }
        stack.clear()
        for (i <- m - 1 to 0 by -1) {
          while (stack.nonEmpty && widthMatrix(stack.top)(j) >= widthMatrix(i)(j)) {
            stack.pop()
          }
          up(i) = if (stack.isEmpty) m else stack.top
          stack.push(i)
        }
        for (i <- 0 until m) {
          val height = up(i) - down(i) - 1
          val area = height * widthMatrix(i)(j)
          maxArea = Math.max(area, maxArea)
        }
      }
      maxArea
    }
  }

  def main(args: Array[String]): Unit = {
    val matrix = Array(
      Array("1", "0", "1", "0", "0"),
      Array("1", "0", "1", "1", "1"),
      Array("1", "1", "1", "1", "1"),
      Array("1", "0", "0", "1", "0")
    ).map(arr => arr.map(_.toCharArray.head))
//    val matrix = Array(
//      Array('1', '1')
//    )
    println(maximalRectangleOpt(matrix))
  }

}
