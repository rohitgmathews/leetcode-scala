package me.rmathews.leetcode.matrix

import scala.annotation.tailrec

/**
  * Solutions to select matrix related questions in leetcode
  */
object Matrix {

  /**
    * Given a m x n matrix, if an element is 0, sets its entire row and column to 0 in place
    * Reference: https://leetcode.com/problems/set-matrix-zeroes/
    * @param matrix An m x n matrix of integers
    */
  def setZeroes(matrix: Array[Array[Int]]): Unit = {
    val numRows = matrix.length
    val numCols = matrix.headOption.map(_.length).getOrElse(0)
    @tailrec
    def go(rowIdx: Int, colIdx: Int, rowsWithZeros: Set[Int], colsWithZeroes: Set[Int]): (Set[Int], Set[Int]) =
      if (rowIdx == numRows) (rowsWithZeros, colsWithZeroes)
      else if (colIdx < numCols && matrix(rowIdx)(colIdx) == 0)
        go(rowIdx, colIdx + 1, rowsWithZeros + rowIdx, colsWithZeroes + colIdx)
      else if (colIdx < numCols)
        go(rowIdx, colIdx + 1, rowsWithZeros, colsWithZeroes)
      else go(rowIdx + 1, 0, rowsWithZeros, colsWithZeroes)
    val (rowsToReset, colsToReset) = go(0, 0, Set.empty[Int], Set.empty[Int])
    // Agreed that this is non-standard in scala, but, the question requires in place modification
    for {
      rowIdx <- matrix.indices
      colIdx <- 0 until numCols
    } {
      if (rowsToReset.contains(rowIdx) || colsToReset.contains(colIdx)) matrix(rowIdx)(colIdx) = 0
    }
  }

  /** Given a matrix of m x n elements (m rows, n columns), returns a seq of elements of the matrix in spiral order
    * Reference: https://leetcode.com/problems/spiral-matrix/
    * @param matrix Input matrix
    * @tparam A Parameter type of the matrix
    */
  def spiralOrder[A](matrix: Array[Array[A]]): List[A] = {
    @tailrec
    def getDown(currRow: Int, currCol: Int, bottomBoundary: Int, acc: List[A]): List[A] =
      if (currRow <= bottomBoundary) getDown(currRow + 1, currCol, bottomBoundary, matrix(currRow)(currCol) :: acc)
      else acc
    @tailrec
    def getRight(currRow: Int, currCol: Int, rightBoundary: Int, acc: List[A]): List[A] =
      if (currCol <= rightBoundary) getRight(currRow, currCol + 1, rightBoundary, matrix(currRow)(currCol) :: acc)
      else acc
    @tailrec
    def getUp(currRow: Int, currCol: Int, topBoundary: Int, acc: List[A]): List[A] =
      if (currRow >= topBoundary) getUp(currRow - 1, currCol, topBoundary, matrix(currRow)(currCol) :: acc)
      else acc
    @tailrec
    def getLeft(currRow: Int, currCol: Int, leftBoundary: Int, acc: List[A]): List[A] =
      if (currCol >= leftBoundary) getLeft(currRow, currCol - 1, leftBoundary, matrix(currRow)(currCol) :: acc)
      else acc

    def go(leftBoundary: Int, rightBoundary: Int, topBoundary: Int, bottomBoundary: Int, acc: List[A]): List[A] = {
      if (leftBoundary == rightBoundary) getUp(bottomBoundary, leftBoundary, topBoundary, acc)
      else if (topBoundary == bottomBoundary) getLeft(topBoundary, rightBoundary, leftBoundary, acc)
      else if (leftBoundary <= rightBoundary && topBoundary <= bottomBoundary) {
        val withLeft = getDown(topBoundary + 1, leftBoundary, bottomBoundary, acc)
        val withBottom = getRight(bottomBoundary, leftBoundary + 1, rightBoundary, withLeft)
        val withRight = getUp(bottomBoundary - 1, rightBoundary, topBoundary, withBottom)
        val withTop = getLeft(topBoundary, rightBoundary - 1, leftBoundary, withRight)
        withTop ::: go(leftBoundary + 1, rightBoundary - 1, topBoundary + 1, bottomBoundary - 1, List.empty[A])
      }
      else acc
    }
    val (numRows, numCols) = getDimensions(matrix)
    go(0, numCols - 1, 0, numRows - 1, List.empty[A])
  }

  private def getDimensions[A](matrix: Array[Array[A]]): (Int, Int) =
    (matrix.length, matrix.headOption.map(_.length).getOrElse(0))
}
