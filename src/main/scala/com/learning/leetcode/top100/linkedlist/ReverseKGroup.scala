package com.learning.leetcode.top100.linkedlist

import com.learning.leetcode.top100.linkedlist.Utils.{ListNode, makeList, tranverse}

object ReverseKGroup {
  def reverse(head: ListNode, tail: ListNode): (ListNode, ListNode) = {
    var prev = tail.next
    var current = head
    while (prev != tail) {
      val currentNext = current.next
      current.next = prev
      prev = current
      current = currentNext
    }
    (tail, head)
  }

  /**
   * 1.添加dummpy node方便遍历，代码更加简洁
   * 2.先遍历找到tail，翻转子链表
   * 3.讲新的子链表和原来的链表头尾相接
   *
   */
  def reverseKGroup(headArg: ListNode, k: Int): ListNode = {
    var head = headArg
    val dummyHead = new ListNode(-1)
    dummyHead.next = head
    var prev = dummyHead
    var continue: Boolean = true
    while (head != null && continue) {
      var tail = prev
      for (i <- 1 to k if continue) {
        tail = tail.next
        if (tail == null) {
          continue = false
        }
      }
      if (continue) {
        val originalTailNext = tail.next
        val (newHead, newTail) = reverse(head, tail)
        prev.next = newHead
        newTail.next = originalTailNext
        prev = newTail
        head = prev.next
      }
    }
    dummyHead.next
  }

  def main(args: Array[String]): Unit = {
    val list = makeList(Array(1, 2, 3, 4, 5))
    val res = reverseKGroup(list, 2)
    tranverse(res)
  }

}
