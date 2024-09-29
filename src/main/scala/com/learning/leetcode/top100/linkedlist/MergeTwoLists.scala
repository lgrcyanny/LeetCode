package com.learning.leetcode.top100.linkedlist

import com.learning.leetcode.top100.linkedlist.Utils.{ListNode, makeList, tranverse}

object MergeTwoLists {

  /**
   * 合并两个有序链表
   * 将两个升序链表合并为一个新的 升序 链表并返回。新链表是通过拼接给定的两个链表的所有节点组成的。
   */

  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {
    if (list1 == null) {
      list2
    } else if (list2 == null) {
      list1
    } else {
      var p1 = list1
      var p2 = list2
      var mergedHead: ListNode = null
      var mergedTail: ListNode = null
      while (p1 != null && p2 != null) {
        val p1Next = p1.next
        val p2Next = p2.next
        if (p1.x < p2.x) {
          if (mergedTail == null) {
            mergedHead = p1
            mergedTail = p1
          } else {
            mergedTail.next = p1
            mergedTail = p1
            p1 = p1Next
          }
        } else {
          if (mergedTail == null) {
            mergedHead = p2
            mergedTail = p2
          } else {
            mergedTail.next = p2
            mergedTail = p2
            p2 = p2Next
          }
        }
      }
      if (p1 != null) {
        mergedTail.next = p1
      }
      if (p2 != null) {
        mergedTail.next = p2
      }
      mergedHead
    }
  }

  def mergeTwoListsRecur(list1: ListNode, list2: ListNode): ListNode = {
    if (list1 == null) {
      list2
    } else if (list2 == null) {
      list1
    } else {
      if (list1.x < list2.x) {
        list1.next = mergeTwoListsRecur(list1.next, list2)
        list1
      } else {
        list2.next = mergeTwoListsRecur(list1, list2.next)
        list2
      }
    }
  }


  def mergeTwoListsOpt(list1: ListNode, list2: ListNode): ListNode = {
    val preHead: ListNode = new ListNode(-1)
    var prev: ListNode = preHead
    var l1: ListNode = list1
    var l2: ListNode = list2
    while (l1 != null && l2 != null) {
      if (l1.x < l2.x) {
        prev.next = l1
        l1 = l1.next
      } else {
        prev.next = l2
        l2 = l2.next
      }
      prev = prev.next
    }
    if (l1 != null) {
      prev.next = l1
    } else {
      prev.next = l2
    }
    preHead.next
  }

  def main(args: Array[String]): Unit = {
    val list1 = makeList(Array(1, 2, 4))
    val list2 = makeList(Array(1, 3, 4, 5))
    val merged = mergeTwoListsOpt(list1, list2)
    tranverse(merged)
  }

}
