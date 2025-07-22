package com.learning.leetcode.top100.tree

import com.learning.leetcode.top100.tree.Utils.TreeNode

object MaxDepth {

  def maxDepth(root: TreeNode): Int = {
    def _getDepth(root: TreeNode, currentDepth: Int): Int = {
      if (root == null) {
        currentDepth
      } else {
        Math.max(_getDepth(root.left, currentDepth + 1), _getDepth(root.right, currentDepth + 1))
      }
    }
    _getDepth(root, 0)
  }


}
