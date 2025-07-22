package com.learning.leetcode.top100.tree

import com.learning.leetcode.top100.tree.Utils.{TreeNode, makeBinaryTree, tranverseTree}

object InvertTree {

  def invertTree(root: TreeNode): TreeNode = {
    if (root == null) {
      root
    } else {
      val left = invertTree(root.left)
      val right = invertTree(root.right)
      root.left = right
      root.right = left
      root
    }
  }

  def main(args: Array[String]): Unit = {
    val root = makeBinaryTree(List(5, 3, 7, 2, 4, 6, 8))
    tranverseTree(root)
    val inverted = invertTree(root)
    tranverseTree(inverted)
  }

}
