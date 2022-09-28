package com.learning.algorithm.graphbfs

import scala.collection.mutable

object BinaryBFS {

  def ladderLengthSingleBFS(beginWord: String, endWord: String, wordList: List[String]): Int = {
    val wordSet = new mutable.HashSet[String]()
    wordList.foreach(w => wordSet.add(w))
    val queue = new mutable.Queue[String]()
    if (wordSet.contains(endWord)) {
      queue.enqueue(beginWord)
    }
    val visited = new mutable.HashSet[String]()
    visited.add(beginWord)
    var step = 0
    var isDone = false
    while (queue.nonEmpty && !isDone) {
      val queueSize = queue.size
      for (i <- 0 until queueSize) {
        val word = queue.dequeue()
        val wordArr = word.toCharArray
        for (j <- 0 until word.size) {
          val originChar = wordArr(j)
          for (k <- 0 until 26) {
            val ch = ('a' + k).toChar
            if (ch != wordArr(j)) {
              wordArr(j) = ch
            }
            val nextWord = wordArr.mkString
            if (nextWord == endWord) {
              isDone = true
            } else if (wordSet.contains(nextWord) && !visited.contains(nextWord)) {
              queue.enqueue(nextWord)
              visited.add(nextWord)
            }
          }
          wordArr(j) = originChar
        }
      }
      step = step + 1
    }
    if (isDone) step + 1 else 0
  }

  def ladderLengthBinaryBFS(beginWord: String, endWord: String, wordList: List[String]): Int = {
    val wordSet = new mutable.HashSet[String]()
    wordList.foreach(w => wordSet.add(w))
    if (!wordSet.contains(endWord) || wordSet.size == 0) {
      0
    } else {
      val visited = new mutable.HashSet[String]()
      var beginVisited = new mutable.HashSet[String]()
      var endVisited = new mutable.HashSet[String]()
      beginVisited.add(beginWord)
      endVisited.add(endWord)
      var step = 0
      var isDone = false
      while (beginVisited.nonEmpty && endVisited.nonEmpty && !isDone) {
        if (beginVisited.size > endVisited.size) {
          val tmp = endVisited
          endVisited = beginVisited
          beginVisited = tmp
        }
        val nextLevelVisited = new mutable.HashSet[String]()
        for (word <- beginVisited) {
          val wordArr = word.toCharArray
          for (j <- 0 until word.size) {
            val originChar = wordArr(j)
            for (k <- 0 until 26) {
              val ch = ('a' + k).toChar
              if (ch != wordArr(j)) {
                wordArr(j) = ch
              }
              val nextWord = wordArr.mkString
              if (wordSet.contains(nextWord)) {
                if (endVisited.contains(nextWord)) {
                  isDone = true
                } else if (!visited.contains(nextWord)) {
                  visited.add(nextWord)
                  nextLevelVisited.add(nextWord)
                }
              }
            }
            wordArr(j) = originChar
          }
        }
        beginVisited = nextLevelVisited
        step = step + 1
      }
      if (isDone) step + 1 else 0
    }
  }

  def main(args: Array[String]): Unit = {
    // beginWord = "hit", endWord = "cog", wordList = ["hot","dot","dog","lot","log","cog"]
    val beginWord = "hit"
    val endWord = "cog"
    val wordList = List("hot","dot","dog","lot","log","cog")
    val step = ladderLengthBinaryBFS(beginWord, endWord, wordList)
    println(step)
  }

}
