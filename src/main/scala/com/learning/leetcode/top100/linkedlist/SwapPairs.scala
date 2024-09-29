package com.learning.leetcode.top100.linkedlist

import com.learning.leetcode.top100.linkedlist.Utils.{ListNode, makeList, tranverse}

object SwapPairs {

  def swapPairs(head: ListNode): ListNode = {
    if (head == null || head.next == null) {
      head
    } else {
      val preHead = new ListNode(-1)
      var prev = preHead
      var first = head
      var second = first.next
      while (second != null) {
        val secondNext = second.next
        prev.next = second
        prev = second
        second.next = first
        first.next = null
        prev = first
        first = secondNext
        second = if (first != null) first.next else null
      }
      if (first != null) {
        prev.next = first
      }
      preHead.next
    }
  }

  def swapPairsRecur(head: ListNode): ListNode = {
    if (head == null || head.next == null) {
      head
    } else {
      val newHead = head.next
      head.next = swapPairsRecur(head.next)
      newHead.next = head
      newHead
    }
  }

  def swapPairsOpt(head: ListNode): ListNode = {
    if (head == null || head.next == null) {
      head
    } else {
      val dummyHead = new ListNode(-1)
      dummyHead.next = head
      var temp = dummyHead
      while (temp.next != null && temp.next.next != null) {
        val first = temp.next
        val second = first.next
        temp.next = second
        first.next = second.next
        second.next = first
        temp = first
      }
      dummyHead.next
    }
  }


  def main(args: Array[String]): Unit = {
    val l = makeList(Array(1, 2, 3, 4, 5, 6))
    val res = swapPairsOpt(l)
    tranverse(res)
  }

}
