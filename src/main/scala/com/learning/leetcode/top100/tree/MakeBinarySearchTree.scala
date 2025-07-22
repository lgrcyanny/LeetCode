package com.learning.leetcode.top100.tree

import com.learning.leetcode.top100.tree.Utils.{TreeNode, tranverseTree}

object MakeBinarySearchTree {

  def sortedArrayToBST(nums: Array[Int]): TreeNode = {
    def _makeTree(left: Int, right: Int): TreeNode = {
      if (left > right) {
        null
      }
      else {
        val middleIndex = (right + left) / 2
        val node = new TreeNode(nums(middleIndex))
        node.left = _makeTree(left, middleIndex - 1)
        node.right = _makeTree(middleIndex + 1, right)
        node
      }
    }
    _makeTree(0, nums.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val root = sortedArrayToBST(Array(-10,-3,0,5,9))
    tranverseTree(root)
  }

}
