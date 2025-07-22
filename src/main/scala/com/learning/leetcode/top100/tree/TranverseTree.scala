package com.learning.leetcode.top100.tree


import com.learning.leetcode.top100.tree.Utils.{TreeNode, makeBinaryTree}

import java.util
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object TranverseTree {

  def preOrderTraversal(root: TreeNode): List[Int] = {
    if (root == null) {
      Nil
    } else {
      val stack = new util.Stack[TreeNode]()
      val res = new ArrayBuffer[Int]()
      stack.push(root)
      while (!stack.isEmpty) {
        val current = stack.pop()
        res.append(current.value)
        if (current.right != null) {
          stack.push(current.right)
        }
        if (current.left != null) {
          stack.push(current.left)
        }
      }
      res.toList
    }
  }

  def inorderTraversal(root: TreeNode): List[Int] = {
    var _root: TreeNode = root
    val stack = new util.Stack[TreeNode]()
    val res = new ArrayBuffer[Int]()
    while (!stack.isEmpty || _root != null) {
      while (_root != null) {
        stack.push(_root)
        _root = _root.left
      }
      _root = stack.pop()
      res.append(_root.value)
      _root = _root.right
    }
    res.toList
  }

  def postOrderTraversal(root: TreeNode): List[Int] = {
    val stack = new util.Stack[TreeNode]()
    val res = new ArrayBuffer[Int]()
    var current: TreeNode = root
    var lastVisited: TreeNode = null
    while (!stack.isEmpty || current != null) {
      if (current != null) {
        stack.push(current)
        current = current.left
      } else {
        val peek = stack.lastElement()
        if (peek.right != null && lastVisited != peek.right) {
          current = peek.right
        } else {
          res.append(peek.value)
          lastVisited = stack.pop()
        }
      }
    }
    res.toList
  }

  def main(args: Array[String]): Unit = {
    val root = makeBinaryTree(List(5, 3, 7, 2, 4, 6, 8))
    // inOrder: 2, 3, 4, 5, 6, 7, 8
    // postOrder: 2, 4, 3, 6, 8, 7, 5
    // preOrder: 5, 3, 2, 4, 7, 6, 8
    val res = preOrderTraversal(root)
    println(res.mkString(", "))
  }

}
