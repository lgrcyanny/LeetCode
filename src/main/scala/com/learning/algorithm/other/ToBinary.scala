package com.learning.algorithm.other

import scala.collection.mutable.ArrayBuffer

/**
  * Created by lgrcyanny on 17/9/1.
  */
object ToBinary {

  def process(n: Long): Array[Byte] = {
    val buffer = new ArrayBuffer[Byte]()
    var p = n
    while (p != 0) {
      if (p % 2 == 0) {
        buffer += 0
      } else {
        buffer += 1
      }
      p = p / 2
    }
    buffer.toArray
  }

  def main(args: Array[String]): Unit = {
    println(process(15).mkString(""))
  }

}
