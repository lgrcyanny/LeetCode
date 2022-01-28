package com.learning.algorithm.graphdfs.dp

object Diameter {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  def diameterOfBinaryTree(root: TreeNode): Int = {
    var res: Int = 0
    def _dfs(node: TreeNode): Int = {
      if (node == null) {
        0
      } else {
        val left = _dfs(node.left)
        val right = _dfs(node.right)
        res = Math.max(res, left + right)
        Math.max(left, right) + 1
      }
    }
    _dfs(root)
    res
  }

}
