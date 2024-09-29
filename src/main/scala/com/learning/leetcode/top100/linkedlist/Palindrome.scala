package com.learning.leetcode.top100.linkedlist

import com.learning.leetcode.top100.linkedlist.Utils._

object Palindrome {

  def reverseList(head: ListNode): ListNode = {
    if (head == null) {
      null
    } else {
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

  def findHalfTail(head: ListNode): ListNode = {
    var slow = head
    var fast = head
    while (fast.next != null && fast.next.next != null) {
      slow = slow.next
      fast = fast.next.next
    }
    slow
  }

  /**
   * 1.用快慢指针找到链表的中间节点，快指针前进一步，慢指针前进两步
   * 2.翻转后半部分链表
   * 3.比较判断是否是回文链表
   * 4.再次反转后半部分
   */
  def isPalindrome(head: ListNode): Boolean = {
    if (head == null) {
      true
    } else {
      val firstHalfTail = findHalfTail(head)
      val secondHalfHead = reverseList(firstHalfTail.next)
      var p1 = head
      var p2 = secondHalfHead
      var result = true
      while (result && p1 != null && p2 != null) {
        if (p1.x != p2.x) {
          result = false
        }
        p1 = p1.next
        p2 = p2.next
      }
      firstHalfTail.next = reverseList(secondHalfHead)
      result
    }
  }

  def main(args: Array[String]): Unit = {
    val head = makeList(Array(1, 2, 3))
    println(isPalindrome(head))
  }

}
