package com.learning.leetcode.top100.linkedlist

import com.learning.leetcode.top100.linkedlist.Utils.{ListNode, makeList, tranverse}

object AddTwoNum {

  /**
   * 给你两个 非空 的链表，表示两个非负的整数。它们每位数字都是按照 逆序 的方式存储的，并且每个节点只能存储 一位 数字。
   * 请你将两个数相加，并以相同形式返回一个表示和的链表。
   */

  /**
   * 0.初始化进位值carry为0
   * 1.遍历链表逐位相加，num = p1.x + p2.x + carry
   * 2.每次把进位保存在carry遍历中
   * 3.当前数位 num % 10, carry 为 num / 10
   * 4.遍历完成后，如果carry>0, 链表后面在加一位
   */
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    val preHead = new ListNode(-1)
    var prev = preHead
    var carry: Int = 0
    var p1 = l1
    var p2 = l2
    while (p1 != null || p2 != null) {
      val num = if (p1 != null && p2 != null) {
        p1.x + p2.x + carry
      } else if (p1 != null) {
        p1.x + carry
      } else {
        p2.x + carry
      }
      val currentBit = num % 10
      carry = num / 10
      prev.next = new ListNode(currentBit)
      prev = prev.next
      p1 = if (p1 != null) p1.next else null
      p2 = if (p2 != null) p2.next else null
    }
    if (carry > 0) {
      prev.next = new ListNode(carry)
    }
    preHead.next
  }

  def main(args: Array[String]): Unit = {
    val l1 = makeList(Array(9, 9, 9, 9, 9, 9, 9))
    val l2 = makeList(Array(9, 9, 9, 9))
    val res = addTwoNumbers(l1, l2)
    tranverse(res)
  }

}
