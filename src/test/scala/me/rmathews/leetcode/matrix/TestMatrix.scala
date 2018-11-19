package me.rmathews.leetcode.matrix

import org.scalatest.FlatSpec

import scala.collection.mutable.ListBuffer

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

  "minimumPathSum" should "calculate the minimum path sum for 2D array" in {
    val input = Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(10, 11, 12, 13)
    )
    val actualOutput = Matrix.getMinimumPathSum(input)
    assertResult(31)(actualOutput)
  }

  "minimumPathSum" should "calculate the minimum path sum for array with 1 row " in {
    val input = Array(
      Array(1, 2, 3, 4)
    )
    val actualOutput = Matrix.getMinimumPathSum(input)
    assertResult(10)(actualOutput)
  }

  "uniquePathWithObstacles" should "return number of paths to destination" in {
    val input = Array(
      Array(false, false, false),
      Array(false, true, false),
      Array(false, false, false)
    )
    val actualResult = Matrix.uniquePathsWithObstacles(input)
    assertResult(2)(actualResult)
  }

  "numberOfIslands" should "return 1 island when there is just 1" in {
    val input = Array(
      Array(1, 1, 1, 1, 0),
      Array(1, 1, 0, 1, 0),
      Array(1, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0)
    )
    val actualResult = Matrix.numberOfIslands(input)
    assertResult(1)(actualResult)
  }

  "numberOfIslands" should "return 3 islands when there are 3" in {
    val input = Array(
      Array(1, 1, 0, 0, 0),
      Array(1, 1, 0, 0, 0),
      Array(0, 0, 1, 0, 0),
      Array(0, 0, 0, 1, 1)
    )
    val actualResult = Matrix.numberOfIslands(input)
    assertResult(3)(actualResult)
  }

  "captureSurroundedRegions" should "surround all regions" in {
    val input = Array(
      Array('X', 'X', 'X', 'X'),
      Array('X', 'O', 'O', 'X'),
      Array('X', 'X', 'O', 'X'),
      Array('X', 'O', 'X', 'X')
    )
    val expected = Array(
      Array('X', 'X', 'X', 'X'),
      Array('X', 'X', 'X', 'X'),
      Array('X', 'X', 'X', 'X'),
      Array('X', 'O', 'X', 'X')
    )
    val actualResult = Matrix.captureSurroundedRegions(input)
      assertResult(expected)(actualResult)
  }

  "captureSurroundedRegions" should "surround no regions" in {
    val input = Array(
      Array('X', 'X', 'O', 'X'),
      Array('X', 'O', 'O', 'X'),
      Array('X', 'X', 'O', 'X'),
      Array('X', 'O', 'X', 'X')
    )
    val expected = Array(
      Array('X', 'X', 'O', 'X'),
      Array('X', 'O', 'O', 'X'),
      Array('X', 'X', 'O', 'X'),
      Array('X', 'O', 'X', 'X')
    )
    val actualResult = Matrix.captureSurroundedRegions(input)
    assertResult(expected)(actualResult)
  }

  "maxAreaInHistogram" should "get max area in regular histogram"  in {
    val input = Array(1, 2, 3, 5, 2, 1)
    val actualResult = Matrix.maxAreaInHistogram(input)
    assertResult(8)(actualResult)
  }

  "maxAreaInHistogram" should "get max area in increasing histogram"  in {
    val input = Array(2, 4, 6, 8)
    val actualResult = Matrix.maxAreaInHistogram(input)
    assertResult(12)(actualResult)
  }

  "maximalRectangle" should "return max area when matrix has multiple rows" in {
    val input = Array(
      Array(0, 0, 1, 1),
      Array(0, 0, 1, 1),
      Array(0, 1, 1, 1),
      Array(0, 1, 1, 1)
    )
    val actualResult = Matrix.maximalRectangle(input)
    assertResult(8)(actualResult)
  }

  "maximalRectangle" should "return max area when matrix has one row" in {
    val input = Array(
      Array(0, 0, 1, 1)
    )
    val actualResult = Matrix.maximalRectangle(input)
    assertResult(2)(actualResult)
  }

  "maximalSquare" should "return square of area 4" in {
    val input = Array(
      Array(0, 0, 0, 0),
      Array(0, 1, 1, 0),
      Array(0, 1, 1, 0)
    )
    val actualResult = Matrix.maximalSquare(input)
    assertResult(4)(actualResult)
  }

  "maximalSquare" should "return square of area 1" in {
    val input = Array(
      Array(1, 0, 0, 0),
      Array(0, 1, 1, 0),
      Array(0, 0, 1, 0)
    )
    val actualResult = Matrix.maximalSquare(input)
    assertResult(1)(actualResult)
  }

}
