package me.rmathews.leetcode.matrix

import org.scalatest.FlatSpec

class TestMatrix extends FlatSpec {

  "setZeroes" should "set rows and columns to 0 as necessary" in {
    val input = Array(
      Array(1, 2, 3, 4, 5),
      Array(6, 7, 8, 0, 10)
    )
    val expectedOutput = Array(
      Array(1, 2, 3, 0, 5),
      Array(0, 0, 0, 0, 0)
    )
    Matrix.setZeroes(input)
    // Counter intuitive, but, is the case given we need to modify in place per question
    assertResult(expectedOutput)(input)
  }

  "spiralOrder" should "return a sequence of elements in matrix in spiral order " in {
    val input = Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(10, 11, 12, 13)
    )
    val expectedOutput = List(1, 2, 3, 4, 8, 13, 12, 11, 10, 5, 6, 7)
    val actualOutput = Matrix.spiralOrder(input)
    assertResult(expectedOutput)(actualOutput)
  }

  "spiralOrder" should "return a sequence of elements in matrix with one row " in {
    val input = Array(
      Array(1, 2, 3, 4, 5)
    )
    val expectedOutput = List(1, 2, 3, 4, 5)
    val actualOutput = Matrix.spiralOrder(input)
    assertResult(expectedOutput)(actualOutput)
  }

  "spiralOrder" should "return a sequence of elements in matrix with one column " in {
    val input = Array(
      Array(1),
      Array(2),
      Array(3)
    )
    val expectedOutput = List(1, 2, 3)
    val actualOutput = Matrix.spiralOrder(input)
    assertResult(expectedOutput)(actualOutput)
  }

  "spiralOrder" should "empty list for empty array " in {
    val input = Array.empty[Array[Int]]
    val actualOutput = Matrix.spiralOrder(input)
    assertResult(List.empty[Int])(actualOutput)
  }

}
