package com.learning.leetcode.top100.linkedlist

import scala.collection.mutable

object LRUCache {

  /**
   * 请你设计并实现一个满足  LRU (最近最少使用) 缓存 约束的数据结构
   * 1.hashmap保存key, value指向双向链表节点
   * 2.链表头部是最近使用的，尾部是最近最少使用的
   * @param _capacity
   */
  class LRUCache(_capacity: Int) {
    val cache = new mutable.HashMap[Int, ListNode]()
    var dummyHead: ListNode = new ListNode(-1, -1)
    var dummyTail: ListNode = new ListNode(-1, -1, dummyHead, null)
    dummyHead.next = dummyTail
    var size: Int = 0
    val capacity: Int = _capacity
    case class ListNode(key: Int, _value: Int,  _prev: ListNode = null, _next: ListNode = null) {
      var value: Int = _value
      var next: ListNode = _next
      var prev: ListNode = _prev
    }

    def get(key: Int): Int = {
      if (cache.contains(key)) {
        val node = cache(key)
        moveToHead(node)
        node.value
      } else {
        -1
      }
    }

    def put(key: Int, value: Int): Unit = {
      if (cache.contains(key)) {
        val node = cache(key)
        node.value = value
        moveToHead(node)
      } else {
        val node = ListNode(key, value)
        cache.put(key, node)
        addToHead(node)
        size = size + 1
        if (size > capacity) {
          val node = removeTail()
          cache.remove(node.key)
          size = size - 1
        }
      }
    }

    private def removeNode(node: ListNode): Unit = {
      node.prev.next = node.next
      node.next.prev = node.prev
    }

    private def addToHead(node: ListNode): Unit = {
      node.prev = dummyHead
      node.next = dummyHead.next
      dummyHead.next.prev = node
      dummyHead.next = node
    }

    private def moveToHead(node: ListNode): Unit = {
      removeNode(node)
      addToHead(node)
    }

    private def removeTail(): ListNode = {
      val node = dummyTail.prev
      removeNode(node)
      node
    }
  }

  def main(args: Array[String]): Unit = {
    val l = new mutable.HashMap[Int, Int]()
    l.put(1, 1)
    l.put(2, 2)
    println(l.size)
  }


}
