package com.learning.leetcode.top100.linkedlist

import com.learning.leetcode.top100.linkedlist.Utils.{ListNode, makeList, tranverse}


object SortList {

  def merge(listA: ListNode, listB: ListNode): ListNode = {
    val dummyNode = new ListNode(-1)
    var prev = dummyNode
    var headA = listA
    var headB = listB
    while (headA != null && headB != null) {
      if (headA.x < headB.x) {
        prev.next = headA
        headA = headA.next
      } else {
        prev.next = headB
        headB = headB.next
      }
      prev = prev.next
    }
    if (headA != null) {
      prev.next = headA
    } else if (headB != null) {
      prev.next = headB
    }
    dummyNode.next
  }

  /**
   * partition list in the middle
   * @param head
   * @return
   */
  def partition(head: ListNode): (ListNode, ListNode) = {
    if (head == null) {
      (null, null)
    } else {
      var p: ListNode = head
      var q: ListNode = p
      while (q.next != null && q.next.next != null) {
        p = p.next
        q = q.next.next
      }
      val headA = head
      val headB = p.next
      p.next = null
      (headA, headB)
    }
  }

  /**
   * 1.先用快慢指针找到中间节点
   * 2.对两个子链表排序，递归调用，停止条件是head为空或只有一个元素。（少了只有一个元素条件时，会stackoverflow）
   * 3.对排序的链表进行merge
   */
  def sortList(head: ListNode): ListNode = {
    if (head == null || head.next == null) {
      head
    } else {
      val (headA, headB) = partition(head)
      val sortedHeadA = sortList(headA)
      val sortedHeadB = sortList(headB)
      val mergedRes = merge(sortedHeadA, sortedHeadB)
      mergedRes
    }
  }

  def main(args: Array[String]): Unit = {
    val list = makeList(Array(0, -1, 9, 10, 7))
    val res = sortList(list)
    tranverse(res)
  }

}
