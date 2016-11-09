package com.learning.algorithm.string

import org.scalatest.{FunSpec, Matchers, WordSpec}

/**
  * Created by lgrcyanny on 16/11/9.
  */
class SubStringAlgorithmTest extends WordSpec with Matchers {

  "String" should {
    "substring indexof" in {
      val str = "abcdefg"
      val index = SubStringAlgorithm.strstr(str, "ef")
      index should be(4)

      SubStringAlgorithm.strstr(str, "fh") should be(-1)
    }
  }

}
