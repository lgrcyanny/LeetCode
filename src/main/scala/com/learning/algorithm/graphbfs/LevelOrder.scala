package com.learning.algorithm.graphbfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LevelOrder {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  def levelOrder(root: TreeNode): List[List[Int]] = {
    val res = new ArrayBuffer[List[Int]]()
    val queue = new mutable.Queue[TreeNode]()
    if (root != null) {
      queue.enqueue(root)
    }
    while (queue.nonEmpty) {
      val currentSize = queue.size
      val output = new ArrayBuffer[Int]()
      for (i <- 0 until currentSize) {
        val front = queue.dequeue()
        output.append(front.value)
        if (front.left != null) {
          queue.enqueue(front.left)
        }
        if (front.right != null) {
          queue.enqueue(front.right)
        }
      }
      res.append(output.toList)
    }
    res.toList
  }

  def main(args: Array[String]): Unit = {

  }

}
