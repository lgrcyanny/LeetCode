package com.learning.leetcode.top100.tree

import com.learning.leetcode.top100.tree.Utils.TreeNode

import scala.collection.mutable

object SymmetricTree {

  /**
   * 递归思路: 两个指针，同时左移和右移，比较值是否相等
   */
  def isSymmetric(root: TreeNode): Boolean = {
    def _check(p: TreeNode, q: TreeNode): Boolean = {
      if (p == null && q == null) {
        true
      } else if (p == null || q == null) {
        false
      } else {
        p.value == q.value && _check(p.left, q.right) && _check(p.right, q.left)
      }
    }
    _check(root.left, root.right)
  }

  /**
   * 1.维护一个队列，每次入队两个元素
   * 2.先入队root，root
   * 3.如果值不相等，就返回false，停止迭代
   * 4.然后入队left, right; right, left一对节点
   */
  def isSymmetricIter(root: TreeNode): Boolean = {
    val queue = new mutable.Queue[TreeNode]()
    queue.enqueue(root)
    queue.enqueue(root)
    var continue: Boolean = true
    var res: Boolean = true
    while (!queue.isEmpty && continue) {
      val p = queue.dequeue()
      val q = queue.dequeue()
      if ((p == null || q == null) || p.value != q.value ) {
        res = false
        continue = false
      } else if (p != null && q != null) {
        queue.enqueue(p.left)
        queue.enqueue(q.right)
        queue.enqueue(p.right)
        queue.enqueue(q.left)
      }
    }
    res
  }


}
