package com.learning.algorithm.graphdfs.dp

object LongestConsecutive {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  def longestConsecutive(root: TreeNode): Int = {
    var res: Int = 0
    def _dfs(node: TreeNode): Int = {
      if (node == null) {
        0
      } else {
        var left = _dfs(node.left) + 1
        var right = _dfs(node.right) + 1
        if (node.left != null && node.value + 1 != node.left.value) {
          left = 1
        }
        if (node.right != null && node.value + 1 != node.right.value) {
          right = 1
        }
        val len = Math.max(left, right)
        res = Math.max(len, res)
        len
      }
    }
    _dfs(root)
    res
  }


}
