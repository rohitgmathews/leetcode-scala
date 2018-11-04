package me.rmathews.leetcode.binarytree

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

object BinaryTree extends App {

  /**
    * A BinaryTree
    */

  sealed trait Tree[+A]
  /**
    * A binary tree node
    * @param left left sub-tree
    * @param entry the value stored at the node
    * @param right right sub-tree
    */
  case class Node[A](entry: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  case object EmptyTree extends Tree[Nothing]

  /*
               18
           /       \
         17         30
        /  \        /  \
      16    50    100   46
     /  \   /
    8   7  9
 */
  val sampleTree: Tree[Int] = Node(18,
    Node(17,
      Node(16,
        Node(8, EmptyTree, EmptyTree),
        Node(7, EmptyTree, EmptyTree)
      ),
      Node(50,
        Node(9, EmptyTree, EmptyTree),
        EmptyTree
      )
    ),
    Node(30,
      Node(100, EmptyTree, EmptyTree),
      Node(46, EmptyTree, EmptyTree)
    )
  )

  /*
              20
           /       \
         15         30
        /  \        /  \
      10    17    25   40
     /  \   /
    8   12 16
   */
  val bstTree: Tree[Int] = Node(20,
    Node(15,
      Node(10,
        Node(8, EmptyTree, EmptyTree),
        Node(12, EmptyTree, EmptyTree)
      ),
      Node(17,
        Node(16, EmptyTree, EmptyTree),
        EmptyTree
      )
    ),
    Node(30,
      Node(25, EmptyTree, EmptyTree),
      Node(40, EmptyTree, EmptyTree)
    )
  )

  def invertTree[A](t: Tree[A]): Tree[A] = t match {
    case EmptyTree => EmptyTree
    case Node(entry, left, right) => Node(entry, invertTree(right), invertTree(left))
  }

  def longestConsecutiveSequence(t: Tree[Int]): Int = {
    def go(t: Tree[Int], longestSoFar: Int): Int  = t match {
      case EmptyTree => longestSoFar
      case Node(entry, left, right) =>
        val longestLeft = left match {
          case EmptyTree => longestSoFar
          case Node(leftEntry, _, _) if entry == leftEntry + 1 => go(left, longestSoFar + 1)
          case _ => go(left, 1)
        }
        val longestRight = right match {
          case EmptyTree => longestSoFar
          case Node(rightEntry, _, _) if entry == rightEntry + 1 => go(right, longestSoFar + 1)
          case _ => go(right, 1)
        }
        longestLeft max longestRight max longestSoFar
    }
    go(t, 1)
  }

  def isTreeBST[A](t: Tree[A])(implicit ordering: Ordering[A]): Boolean = {
    def go(tr: Tree[A], minAllowed: Option[A], maxAllowed: Option[A]): Boolean = tr match {
      case EmptyTree => true
      case Node(entry, left, right) =>
        minAllowed.forall(min => ordering.gt(entry, min)) &&
        maxAllowed.forall(max => ordering.lt(entry, max)) &&
        go(left, minAllowed, Some(entry)) &&
        go(right, Some(entry), maxAllowed)
    }
    go(t, None, None)
  }

  def hasRootToLeafSum(t: Tree[Int], requiredSum: Int): Boolean = {
    @tailrec
    def go(queue: Queue[(Tree[Int], Int)]): Boolean = {
      if (queue.isEmpty) false
      else {
        val ((tree, sumAlongPath), q) = queue.dequeue
        tree match {
          case Node(entry, left, right) => go(q.enqueue((left, sumAlongPath + entry)).enqueue((right, sumAlongPath + entry)))
          case EmptyTree if sumAlongPath == requiredSum => true
          case _ => go(q)
        }
      }
    }
    val empty = Queue.empty[(Tree[Int], Int)]
    go(empty.enqueue((t, 0)))
  }

  def getRootToLeafPathsWithSum(t: Tree[Int], requiredSum: Int): List[List[Int]] = {
    @tailrec
    def go(queue: Queue[(Tree[Int], Int, List[Int])], acc: List[List[Int]]): List[List[Int]] = queue match {
      case (tree, sumAlongPath, numsAlongPath) +: q =>
        tree match {
          case EmptyTree => go(q, acc)
          case Node(entry, EmptyTree, EmptyTree) if sumAlongPath + entry == requiredSum => go(q, (entry :: numsAlongPath) :: acc)
          case Node(_, EmptyTree, EmptyTree) => go(q, acc)
          case Node(entry, left, right) => go(q.enqueue((left, sumAlongPath + entry, entry :: numsAlongPath)).enqueue((right, sumAlongPath + entry, entry :: numsAlongPath)), acc)
        }
      case _ => acc
    }
    val empty = Queue.empty[(Tree[Int], Int, List[Int])]
    go(empty.enqueue((t, 0, List.empty[Int])), List.empty[List[Int]])
  }

  def treeFrom[A](inOrder: List[A], preOrder: List[A]): Tree[A] = preOrder match {
    case Nil => EmptyTree
    case head :: tail =>
      val (leftIO, _ :: rightIO) = inOrder.splitAt(inOrder.indexOf(head))
      val (leftPre, rightPre) = tail.splitAt(leftIO.size)
      Node(head, treeFrom(leftIO, leftPre), treeFrom(rightIO, rightPre))
  }

  def treeFrom[A](inOrder: List[A], postOrder: Array[A]): Tree[A] = {
    if (postOrder.isEmpty) EmptyTree
    else {
      val (leftIO, _ :: rightIO) = inOrder.splitAt(inOrder.indexOf(postOrder.last))
      val (leftPost, rightPost) = postOrder.dropRight(1).splitAt(leftIO.length)
      Node(postOrder.last, treeFrom(leftIO, leftPost), treeFrom(rightIO, rightPost))
    }
  }

  def preOrder[A](t: Tree[A]): List[A] = t match {
    case EmptyTree => Nil
    case Node(entry, left, right) => entry :: preOrder(left) ::: preOrder(right)
  }

  def inOrder[A](t: Tree[A]): List[A] = t match {
    case EmptyTree => Nil
    case Node(entry, left, right) => inOrder(left) ::: (entry :: inOrder(right))
  }

  def postOrder[A](t: Tree[A]): List[A] = t match {
    case EmptyTree => Nil
    case Node(entry, left, right) => postOrder(left) ::: postOrder(right) ::: List(entry)
  }

  def bstFrom[A](arr: Array[A]): Tree[A] = {
    def helper(startIdx: Int, endIdx: Int): Tree[A] = {
      if (startIdx > endIdx) EmptyTree
      else {
        val midIdx = (startIdx + endIdx) / 2
        Node(arr(midIdx), helper(startIdx, midIdx - 1), helper(midIdx + 1, endIdx))
      }
    }
    helper(0, arr.length - 1)
  }

  def minimumDepth[A](t: Tree[A]): Int = {
    @tailrec
    def go(queue: Queue[(Tree[A], Int)]): Int = queue match {
      case (tree, depthOfNode) +: dequeued => tree match {
        case EmptyTree => go(dequeued)
        case Node(_, EmptyTree, EmptyTree) => depthOfNode
        case Node(_, left, right) => go(dequeued.enqueue((left, depthOfNode + 1)).enqueue((right, depthOfNode + 1)))
      }
      case _ => 0
    }
    go(Queue((t, 1)))
  }

  def maximumPathSum(t: Tree[Int]): Int = {
    var maxSoFar = Integer.MIN_VALUE
    def helper(t: Tree[Int]): Int = t match {
      case EmptyTree => 0
      case Node(entry, left, right) =>
        val maxLeft = helper(left)
        val maxRight = helper(right)
        val result = Math.max(maxLeft + entry, entry + maxRight)
        maxSoFar = Math.max(maxSoFar, maxLeft + entry + maxRight)
        result
    }
    helper(t)
    maxSoFar
  }

  def isBalanced(t: Tree[Int]): Boolean = {
    def go(t: Tree[Int]): (Int, Boolean) = t match {
      case EmptyTree => (0, true)
      case Node(_, left, right) =>
        val (heightOfLeft, isLeftBalanced) = go(left)
        val (heightOfRight, isRightBalanced) = go(right)
        (Math.max(heightOfLeft, heightOfRight) + 1, isLeftBalanced && isRightBalanced && Math.abs(heightOfLeft - heightOfRight) < 2)
    }
    go(t)._2
  }

  /*
               18
           /       \
         17         17
        /  \        /  \
      16    50    50   16

  */
  val symmetricTree: Tree[Int] = Node(18,
    Node(17,
      Node(16, EmptyTree, EmptyTree),
      Node(50, EmptyTree, EmptyTree)
    ),
    Node(17,
      Node(50, EmptyTree, EmptyTree),
      Node(16, EmptyTree, EmptyTree)
    )
  )

  def isTreeSymmetric[A](t: Tree[A]): Boolean = {
    def helper(l: Tree[A], r: Tree[A]): Boolean = (l, r) match {
      case (EmptyTree, EmptyTree) => true
      case (Node(lEntry, lLeft, lRight), Node(rEntry, rLeft, rRight)) =>
        (lEntry == rEntry) && helper(lLeft, rRight) && helper(lRight, rLeft)
      case _ => false
    }
    t match {
      case EmptyTree => true
      case Node(_, l, r) => helper(l, r)
    }
  }

  def rightSideView[A](t: Tree[A]): Queue[A] = {
    @tailrec
    def helper(q: Queue[(Tree[A], Int)], acc: Queue[A], prevNodeDepth: Int): Queue[A] = q match {
      case (tree, currNodeDepth) +: dequeued => tree match {
        case Node(entry, leftNode, rightNode)=>
          val newAcc = if(currNodeDepth == prevNodeDepth + 1) acc :+ entry else acc
          val enqueued = List(rightNode, leftNode).foldLeft(dequeued) {
            case (qSoFar, EmptyTree) => qSoFar
            case (qSoFar, n) => qSoFar.enqueue((n, currNodeDepth + 1))
          }
          helper(enqueued, newAcc, currNodeDepth)
        case EmptyTree =>
          helper(dequeued, acc, currNodeDepth)
      }
      case _ => acc
    }
    t match {
      case EmptyTree => Queue.empty[A]
      case _ => helper(Queue((t, 1)), Queue.empty[A], 0)
    }
  }

  def lowestCommonAncestor[A](root: Tree[A], p: A, q: A): Tree[A] = root match {
    case EmptyTree => EmptyTree
    case t @ Node(entry, left, right) =>
      val l = lowestCommonAncestor(left, p, q)
      val r = lowestCommonAncestor(right, p, q)
      if (l == EmptyTree && r == EmptyTree) {
        if (entry == p) t
        else if (entry == q) t
        else EmptyTree
      }
      else if (l == EmptyTree) r
      else if (r == EmptyTree) l
      else t
  }

  def isPreOrderValid(preOrder: List[String]): Boolean = {
    def go(preOrder: List[String], stack: List[String]): List[String] = stack match {
      case "#"::"#":: value :: remaining if !value.equals("#") =>
        go(preOrder, "#" :: remaining)
      case _ if preOrder.nonEmpty => go(preOrder.tail, preOrder.head :: stack)
      case _ => stack
    }
    go(preOrder, List.empty[String]) match {
      case "#" :: Nil => true
      case _ => false
    }
  }

  def sumRootToLeaf(t: Tree[Int]): List[Int] = {
    @tailrec
    def go(q: Queue[(Tree[Int], Int)], acc: List[Int]): List[Int] = q match {
      case (tree, sumSoFar) +: dequeued => tree match {
        case Node(entry, EmptyTree, EmptyTree) =>
          go(dequeued, (entry + sumSoFar) :: acc)
        case Node(entry, left, right) =>
          val enqueued = List(left, right).foldLeft(dequeued) {
            case (qSoFar, EmptyTree)  => qSoFar
            case (qSoFar, tr) => qSoFar.enqueue((tr, sumSoFar + entry))
          }
          go(enqueued, acc)
        case _ => go(dequeued, acc)
      }
      case _ => acc
    }
    t match {
      case EmptyTree => List.empty[Int]
      case _ => go(Queue((t, 0)), List.empty[Int])
    }
  }

  def getClosestValue(t: Tree[Int], to: Int): Int = {
    @tailrec
    def go(t: Tree[Int], to: Int, closestSoFar: Int): Int = t match {
      case EmptyTree => closestSoFar
      case Node(entry, left, right) =>
        val closest = if (Math.abs(entry - to) < Math.abs(to - closestSoFar)) entry else closestSoFar
        if (entry < to) go(right, to, closest)
        else if (entry > to) go(left, to, closest)
        else to
    }
    go(t, to, Integer.MAX_VALUE)
  }

  def preOrderWithGaps[A](t: Tree[A]): Array[Option[A]] = {
   def go(l: List[Tree[A]], acc: Array[Option[A]]): Array[Option[A]] = l match {
     case Nil => acc
     case head :: tail =>
       head match {
         case EmptyTree => go(tail, acc :+ None)
         case Node(entry, left, right) => go (left :: right :: tail, acc :+ Some(entry))
       }
   }
    go(List(t), Array.empty[Option[A]])
  }

  def recoverFromPreOrder[A](arr: Array[Option[A]]): Tree[A] = {
    var currIdx = 0
    def go(): Tree[A] = arr(currIdx) match {
      case None => EmptyTree
      case Some(entry) =>
        currIdx =  currIdx + 1
        val left = go()
        currIdx = currIdx + 1
        val right = go()
        Node(entry, left, right)
    }
    go()
  }

  def levelOrderWithGaps[A](t: Tree[A]): Array[Option[A]] = {
    @tailrec
    def go(q: Queue[Tree[A]], acc: Array[Option[A]]): Array[Option[A]] = q match {
      case EmptyTree +: dequeued => go(dequeued, acc :+ None)
      case Node(entry, left, right) +: dequeued => go(dequeued.enqueue(left).enqueue(right), acc :+ Some(entry))
      case _ => acc
    }
    go(Queue(t), Array.empty[Option[A]])
  }

  def recoverFromLevelOrder[A](levelOrder: Array[Option[A]]): Tree[A] = {
    def go(currIdx: Int): Tree[A] = levelOrder(currIdx) match {
      case None => EmptyTree
      case Some(entry) => Node(entry, go(2 * currIdx + 1), go(2 * currIdx + 2))
    }
    go(0)
  }

  def inOrderSuccessor[A](t: Tree[A], needle: A)(implicit ord: Ordering[A]): Option[Tree[A]] = {
    def getLeftMost(tr: Tree[A]): Tree[A] = tr match {
      case Node(_, EmptyTree, _) => tr
      case Node(_, l, _) => getLeftMost(l)
    }
    def go(t: Tree[A], prev: Option[Tree[A]]): Option[Tree[A]] = t match {
      case EmptyTree => None
      case curr @ Node(entry, left, right) =>
        if (ord.lt(entry, needle)) go(right, prev)
        else if (ord.gt(entry, needle)) go (left, Some(curr))
        else {
          if (right == EmptyTree) prev
          else Some(getLeftMost(right))
        }
    }
    go(t, None)
  }

  def collectLeaves[A](t: Tree[A]): List[List[A]] = {
    var results = mutable.Map.empty[Int, List[A]]
    def go(t: Tree[A]): Int = t match {
      case EmptyTree => -1
      case Node(entry, left, right) =>
        val leftHeight = go(left)
        val rightHeight = go(right)
        val curHeight = Math.min(leftHeight, rightHeight) + 1
        if (results.contains(curHeight)) {
          val newList = entry :: results(curHeight)
          results += (curHeight -> newList)
        }
        else {
          results += (curHeight -> List(entry))
        }
        curHeight
    }
    go(t)
    results.values.toList
  }


  println(sampleTree)
  println(s"Tree from\n${treeFrom(inOrder(sampleTree), preOrder(sampleTree))}")
  println(s"Tree from\n${treeFrom(inOrder(sampleTree), postOrder(sampleTree).toArray)}")
  println(invertTree(sampleTree))
  println(longestConsecutiveSequence(sampleTree))
  println(s"is sampleTree bst: ${isTreeBST(sampleTree)}")
  println(s"is bstTree bst: ${isTreeBST(bstTree)}")
  println(s"Has root to leaf sum: ${hasRootToLeafSum(sampleTree, 148)}")
  println(s"Has root to leaf sum: ${hasRootToLeafSum(sampleTree, 48)}")
  println(s"Has root to leaf sum: ${hasRootToLeafSum(sampleTree, 94)}")
  println(s"Has root to leaf sum: ${getRootToLeafPathsWithSum(sampleTree, 94)}")
  println(s"preOrder of sample tree: ${preOrder(sampleTree)}")
  println(s"postOrder of sample tree: ${postOrder(sampleTree)}")
  println(s"inOrder of bst tree: ${inOrder(bstTree)}")
  println(s"Tree from ${treeFrom(inOrder(sampleTree), preOrder(sampleTree))}")
  println(s"BST From ${bstFrom(Array(1,2,3,4,5,6,7,8))}")
  println(s"Minimum depth of sample tree is ${minimumDepth(sampleTree)}")
  println(s"Maximum path sum ${maximumPathSum(sampleTree)}")
  println(s"isBalanced ${isBalanced(sampleTree)}")
  println(s"Is symmetric symmetricTree : ${isTreeSymmetric(symmetricTree)}, is sym sampleTree: ${isTreeSymmetric(sampleTree)}")
  println(s"Right side view of sample tree: ${rightSideView(sampleTree)}")
  println(s"Lowest common ancestor of 8 and 9 in sample tree : ${lowestCommonAncestor(sampleTree, 8, 9)}")
  println(s"Is pre-order valid : ${isPreOrderValid(List("9","3","4","#","#","1","#","#","2","#","6","#","#"))}")
  println(s"Is pre-order valid : ${isPreOrderValid(List("9","3","4","#","#","1","#","#","2","#","6","#"))}")
  println(s"Sum root to leaf paths: ${sumRootToLeaf(sampleTree)}")
  println(s"Closest value to 31 in bst: ${getClosestValue(bstTree, 31)}")

  println(s"Pre order with gaps: ${preOrderWithGaps(sampleTree).toList}")
  println(sampleTree)
  println(s"Recovered from pre-order: ${recoverFromPreOrder(preOrderWithGaps(sampleTree))}")
  println(s"Level order with gaps: ${levelOrderWithGaps(sampleTree).toList}")
  println(s"Recovered from level-order: ${recoverFromLevelOrder(levelOrderWithGaps(sampleTree))}")
  println(s"In order successor of 15, 16, 17, 40 are : ${inOrderSuccessor(bstTree, 15)}, ${inOrderSuccessor(bstTree, 16)}, ${inOrderSuccessor(bstTree, 17)}, ${inOrderSuccessor(bstTree, 20)}")
  println(s"Collecting leaves: ${collectLeaves(sampleTree)}")
}