package com.learning.algorithm.graph2.tree

import scala.collection.mutable

object MinDepth {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  def minDepth(root: TreeNode): Int = {
    def _recur(node: TreeNode, depth: Int): Int = {
      if (node == null) {
        depth - 1
      } else if (node.left == null && node.right == null) {
        depth
      } else if (node.left == null) {
        _recur(node.right, depth + 1)
      } else if (node.right == null) {
        _recur(node.left, depth + 1)
      } else {
        Math.min(_recur(node.left, depth + 1), _recur(node.right, depth + 1))
      }
    }
    _recur(root, 1)
  }

  def minDepthDFS(root: TreeNode): Int = {
    def _recur(node: TreeNode): Int = {
      if (node == null) {
        0
      } else {
        val minLeft = _recur(node.left)
        val minRight = _recur(node.right)
        if (node.left == null || node.right == null) {
          minLeft + minRight + 1
        } else {
          Math.min(minLeft, minRight) + 1
        }
      }
    }
    _recur(root)
  }

  /**
   * return the minDepth when find first leaf node
   */
  def minDepthBFS(root: TreeNode): Int = {
    if (root == null) {
      0
    } else {
      val queue = new mutable.Queue[(TreeNode, Int)]()
      queue.enqueue((root, 1))
      var minRes = Int.MaxValue
      var isDone = false
      while (queue.nonEmpty && !isDone) {
        val (node, depth) = queue.dequeue()
        if (node.left == null && node.right == null) {
          minRes = depth
          isDone = true
        }
        if (node.left != null) {
          queue.enqueue((node.left, depth + 1))
        }
        if (node.right != null) {
          queue.enqueue((node.right, depth + 1))
        }
      }
      minRes
    }
  }

}
