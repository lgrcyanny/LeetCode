package com.learning.leetcode.top100.linkedlist

import com.learning.leetcode.top100.linkedlist.Utils._
object ReverseList {

  def reverseList(head: ListNode): ListNode = {
    if (head == null) {
      null
    }  else {
      var prev: ListNode = null
      var current = head
      while (current != null) {
        val currentNext = current.next
        current.next = prev
        prev = current
        current = currentNext
      }
      prev
    }
  }

  def reverseListRecur(head: ListNode): ListNode = {
    if (head == null || head.next == null) {
      head
    } else {
      val newHead = reverseListRecur(head.next)
      head.next.next = head
      head.next = null
      newHead
    }
  }

  def main(args: Array[String]): Unit = {
    val mylist = makeList(Array(1, 2))
    tranverse(mylist)
    val reversed = reverseListRecur(mylist)
    tranverse(reversed)
  }

}
