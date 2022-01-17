package com.learning.algorithm.graph2.tree

object MaxDepth {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  def maxDepth(root: TreeNode): Int = {
    def _dfs(node: TreeNode, depth: Int): Int = {
      if (node == null) {
        depth
      } else {
        Math.max(_dfs(node.left, depth + 1), _dfs(node.right, depth + 1))
      }
    }
    _dfs(root, 0)
  }


}
