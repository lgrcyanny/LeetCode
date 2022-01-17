package com.learning.algorithm.graph2.tree

import scala.collection.mutable

object PathSum {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  def hasPathSum(root: TreeNode, targetSum: Int): Boolean = {
    def _recur(node: TreeNode, targetSum: Int): Boolean = {
      if (node == null) {
        false
      } else if (node.left == null && node.right == null) {
        node.value == targetSum
      } else {
        _recur(node.left, targetSum - node.value) ||
          _recur(node.right, targetSum - node.value)
      }
    }
    _recur(root, targetSum)
  }

  def hasPathSumBFS(root: TreeNode, targetSum: Int): Boolean = {
    if (root == null) {
      false
    } else {
      val queue = new mutable.Queue[(TreeNode, Int)]()
      queue.enqueue((root, 0))
      var hasSum = false
      while (queue.nonEmpty && !hasSum) {
        val (node, sum) = queue.dequeue()
        if (node.left == null && node.right == null) {
          if (sum + node.value == targetSum) {
            hasSum = true
          }
        }
        if (node.left != null) {
          queue.enqueue((node.left, node.value + sum))
        }
        if (node.right != null) {
          queue.enqueue((node.right, node.value + sum))
        }
      }
      hasSum
    }
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(5,
      new TreeNode(4,
        new TreeNode(11,
          new TreeNode(7, null, null),
          new TreeNode(2, null, null))), new TreeNode(8))
    println(hasPathSumBFS(root, 22))
  }

}
