package com.learning.leetcode.top100.linkedlist

import com.learning.leetcode.top100.linkedlist.Utils.{ListNode, makeList, tranverse}

object RemoveNthFromEnd {

  /**
   * 删除链表倒数第N个节点
   * 思路：
   * 双指针
   * 1.first和second之间间隔n，同时往后遍历，first到尾部，second就是第n个节点
   * 2.second初始化时指向dummy node, 方便做删除操作
   */
  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    val dummyNode = new ListNode(-1)
    dummyNode.next = head
    var second = dummyNode
    var first = head
    var step = n
    while (first != null && step > 0) {
      first = first.next
      step = step - 1
    }
    while (first != null) {
      second = second.next
      first = first.next
    }
    second.next = second.next.next
    dummyNode.next
  }

  def main(args: Array[String]): Unit = {
    val list = makeList(Array(1))
    val removed = removeNthFromEnd(list, 1)
    tranverse(removed)
  }

}
