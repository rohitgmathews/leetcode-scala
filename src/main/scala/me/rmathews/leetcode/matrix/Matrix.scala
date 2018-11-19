package me.rmathews.leetcode.matrix

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Solutions to select matrix related questions in leetcode
  */
object Matrix {
  import implicits._

  type Matrix[A] = Array[Array[A]]

  /**
    * Given a m x n matrix, if an element is 0, sets its entire row and column to 0 in place
    *
    * Reference: [[https://leetcode.com/problems/set-matrix-zeroes/]]
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
    *
    * Reference: [[https://leetcode.com/problems/spiral-matrix/]]
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

  /** Returns the minimum path sum while traversing a matrix from top left to bottom right
    *
    * Reference: [[https://leetcode.com/problems/minimum-path-sum/]]
    * @param matrix Input matrix
    */
  def getMinimumPathSum(matrix: Matrix[Int]): Int = {
    val (numRows, numCols) = getDimensions(matrix)
    val memo = mutable.Map.empty[(Int, Int), Int]
    for {
      rowIdx <- 0 until numRows
      colIdx <- 0 until numCols
    } (rowIdx, colIdx) match {
      case (0, 0) => memo.update((0, 0), matrix(0)(0))
      case (0, _) =>
        // In the first row, sum to current cell is sum of all cells to that point
        memo.update((rowIdx, colIdx), matrix(rowIdx)(colIdx) + memo(rowIdx, colIdx - 1))
      case (_, 0) =>
        // In the first column, sum to current cell is sum of all cells along that column to that cell
        memo.update((rowIdx, colIdx), matrix(rowIdx)(colIdx) + memo(rowIdx - 1, colIdx))
      case _ =>
        // Sum path to cell is minimum of path to cell above and path to cell on its left
        memo.update(
        (rowIdx, colIdx),
        matrix(rowIdx)(colIdx) + Math.min(memo(rowIdx, colIdx - 1), memo(rowIdx - 1, colIdx))
      )
    }
    if (memo.isEmpty) 0
    else memo(numRows - 1, numCols - 1)
  }

  /** Returns the number of unique paths from top left to bottom right
    *
    * Reference: [[https://leetcode.com/problems/unique-paths-ii/]]
    * @param matrix Input matrix
    * @return
    */
  def uniquePathsWithObstacles(matrix: Matrix[Boolean]): Int = {
    val (numRows, numCols) = getDimensions(matrix)
    val memo = mutable.Map.empty[(Int, Int), Int]
    for {
      rowIdx <- 0 until numRows
      colIdx <- 0 until numCols
    } (rowIdx, colIdx) match {
      case (0, 0) => memo.update((0, 0), if (!matrix(0)(0)) 1 else 0)
      case (0, _) =>
        // In the first row, current cell is reachable if all the cells till here are reachable
        memo.update((rowIdx, colIdx), if (!matrix(rowIdx)(colIdx)) memo(rowIdx, colIdx - 1) else 0)
      case (_, 0) =>
        // In the first column, current cell is reachable if all the cells till here are reachable
        memo.update((rowIdx, colIdx), if (!matrix(rowIdx)(colIdx)) memo(rowIdx - 1, colIdx) else 0)
      case _ =>
        // Number of paths to any other cell is number of paths to cell above and paths to cell on its left
        memo.update(
          (rowIdx, colIdx),
          if (!matrix(rowIdx)(colIdx)) memo(rowIdx, colIdx - 1) + memo(rowIdx - 1, colIdx) else 0
        )
    }
    if (memo.isEmpty) 0
    else memo(numRows - 1, numCols - 1)
  }

  /** Returns the number of dis-joint sets of 1 in matrix comprising of 1's and 0's
    *
    * Reference: [[https://leetcode.com/problems/number-of-islands/]]
    * @param matrix Input matrix
    * @return
    */
  def numberOfIslands(matrix: Matrix[Int]): Int = {
    val (numRows, numCols) = getDimensions(matrix)
    val numCells = numRows * numCols
    def getRoots(startIdx: Int,
                 currIdx: Int,
                 acc: Map[Int, Int],
                 visited: Set[Int]): (Map[Int, Int], Set[Int]) = {
      if (currIdx < 0 || currIdx > numCells - 1) (acc, visited + currIdx)
      else if (matrix.get(currIdx) == 0) (acc, visited + currIdx)
      else {
        val (left, vLeft) = if (!visited.contains(currIdx - 1)) getRoots(startIdx, currIdx - 1, acc, visited + currIdx) else (acc, visited + currIdx)
        val (right, vRight) = if (!vLeft.contains(currIdx + 1)) getRoots(startIdx, currIdx + 1, left, vLeft + currIdx) else (left, vLeft + currIdx)
        val (top, vTop) = if (!vRight.contains(currIdx - numCols)) getRoots(startIdx, currIdx - numCols, right, vRight + currIdx) else (right, vRight + currIdx)
        val (down, vDown) = if (!vTop.contains(currIdx + numCols)) getRoots(startIdx, currIdx + numCols, top, vTop + currIdx) else (top, vTop + currIdx)
        (
          down + (currIdx -> startIdx),
          vDown
        )
      }
    }
    val (uf, _) = (0 until numCells).foldLeft((Map.empty[Int, Int], Set.empty[Int])) { case ((mapSoFar, visitedSoFar), currIdx) =>
      if (!visitedSoFar.contains(currIdx)) getRoots(currIdx, currIdx, mapSoFar, visitedSoFar)
      else (mapSoFar, visitedSoFar)
    }
    uf.values.toSet.size
  }

  /** Returns a new matrix after capturing all surrounded regions
    *
    * Reference: [[https://leetcode.com/problems/surrounded-regions/]]
    * Intuition: All nodes we can reach from a border cell which is an 'O' is not surrounded by 'X'.
    * For each of these we do a depth first search, and, mark them as 'O' in result array. We then mark remaining as 'X'
    * @param matrix Input matrix
    */
  def captureSurroundedRegions(matrix: Matrix[Char]): Matrix[Char] = {
    val (numRows, numCols) = getDimensions(matrix)
    val numCells = numRows * numCols
    val result = Array.fill(numRows, numCols)('-')
    def isBorderElement(idx: Int): Boolean =
      idx < numCols ||                  // first row
      idx > (numCells - numCols) ||     // last row
      idx % numCols == 0 ||             // left border
      (idx + 1) % numCols == 0          // right border
    def dfs(currIdx: Int): Unit =
      if (currIdx >= 0 && currIdx < numCells && result.get(currIdx) == '-') {
        if (matrix.get(currIdx) == 'O') {
          result(currIdx / numCols)(currIdx % numCols) = 'O'
          dfs(currIdx - 1)
          dfs(currIdx + 1)
          dfs(currIdx - numCols)
          dfs(currIdx + numCols)
        }
      }
    for {
      idx <- 0 until numCells
      if isBorderElement(idx) && matrix.get(idx) == 'O'
    } dfs(idx)
    for {
      rowIdx <- 0 until numRows
      colIdx <- 0 until numCols
      if result(rowIdx)(colIdx) != 'O'
    } {
      result(rowIdx)(colIdx) = 'X'
    }
    result
  }

  /** Returns the area of maximal rectangle formed by 1's in matrix of 1's and 0's
    *
    * Reference: [[https://leetcode.com/problems/maximal-rectangle/]]
    * @param matrix Input matrix
    */
  def maximalRectangle(matrix: Matrix[Int]): Int =
    // Intuition: We can treat every row as a histogram, and calculate the max area under said histogram
    matrix match {
      case Array(firstRow, remaining @ _*) =>
        val areaUnderFirstRow = maxAreaInHistogram(firstRow)
        val (_, result) = remaining.foldLeft((firstRow, areaUnderFirstRow)) { case ((heightAbove, maxSoFar), currentRow) =>
          val currentHistogram = heightAbove.zip(currentRow) map {
            case (_, 0) => 0
            case (x, y) => x + y
          }
          (currentHistogram, Math.max(maxSoFar, maxAreaInHistogram(currentHistogram)))
        }
        result
      case _ => 0
    }

  def maxAreaInHistogram(hist: Array[Int]): Int = {
    @tailrec
    def go(currIdx: Int, barsWithLowerHeight: List[Int], maxArea: Int): Int =
      if (currIdx < hist.length) barsWithLowerHeight match {
        case Nil => go(currIdx + 1, List(currIdx), maxArea)
        case head :: _ if hist(currIdx) >= hist(head) => go(currIdx + 1, currIdx :: barsWithLowerHeight, maxArea)
        case head :: tail =>
          /* For every rectangle with height greater than current, calculate area formed by rectangle
             where the greater rectangle is the height of the rectangle. The right boundary (first element on right
             with a lower height) for such a rectangle is this node, and the left boundary is the element
             in the stack before said rectangle.
          */
          val width = currIdx - tail.headOption.getOrElse(-1) - 1
          val currentArea = width * hist(head)
          go(currIdx, tail, Math.max(currentArea, maxArea))
      }
      else barsWithLowerHeight match {
        case Nil => maxArea
        case head :: tail =>
          val width = currIdx - tail.headOption.getOrElse(-1) - 1
          val currentArea = width * hist(head)
          go(currIdx, tail, Math.max(currentArea, maxArea))
      }
    go(0, List.empty[Int], 0)
  }

  /** Returns the area of largest square of 1's in a matrix of 1's and 0's
    *
    * Reference: [[https://leetcode.com/problems/maximal-square/]]
    * @param matrix Input matrix
    */
  def maximalSquare(matrix: Matrix[Int]): Int = matrix match {
    /* Intuition: The size of square formed at (i)(j) =
         if (matrix(i)(j) == 0) = 0
         else 1 + min(sq(i-1)(j), sq(i-1)(j-1), sq(i)(j-1)) */
    case Array(firstRow, remaining @ _*) =>
      val maxSquareInFirstRow = if (firstRow contains 1) 1 else 0
      val (_, maxArea) = remaining.foldLeft((firstRow, maxSquareInFirstRow)) { case ((maxSquaresInPrevRow, maxSoFar), currentRow) =>
        val maxSquaresInCurrentRow = Array.fill(maxSquaresInPrevRow.length)(currentRow(0))
        val result = (1 until firstRow.length).foldLeft(Math.max(maxSoFar, currentRow(0))) { case (m, currIdx) =>
          val currentCell =
            if (currentRow(currIdx) == 0) 0
            else 1 + (maxSquaresInPrevRow(currIdx - 1) min maxSquaresInPrevRow(currIdx) min maxSquaresInCurrentRow(currIdx - 1))
          maxSquaresInCurrentRow(currIdx) = currentCell
          Math.max(m, currentCell * currentCell)
        }
        (maxSquaresInCurrentRow, result)
      }
      maxArea
    case _ => 0
  }

  def getDimensions[A](matrix: Array[Array[A]]): (Int, Int) =
    (matrix.length, matrix.headOption.map(_.length).getOrElse(0))

  private def get2DIndices(idx: Int, numCols: Int): (Int, Int) = (idx / numCols, idx % numCols)

  object implicits {
    implicit class MatrixWithGet[A](matrix: Matrix[A]) {
      def get(idx: Int): A = {
        val (rowIdx, colIdx) = get2DIndices(idx, matrix.headOption.map(_.length).getOrElse(0))
        matrix(rowIdx)(colIdx)
      }
    }
  }




}
