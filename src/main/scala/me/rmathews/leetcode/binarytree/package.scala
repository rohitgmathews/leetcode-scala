package me.rmathews.leetcode

/** Provides an implementation of a Simple Binary Tree in scala.
  * Also provides solutions for select questions from leetcode in Scala
  *
  * ==Overview==
  * The implementation of a Binary Tree and solutions to the problems that leverage are all present in
  * [[me.rmathews.leetcode.binarytree.BinaryTree]]
  *
  * The code can be divided into 4 parts:
  *
  * ===Implementation for a simple binary tree===
  * {{{
  *   sealed trait Tree[+A]
  *   case class Node[A](entry: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  *   case object EmptyTree extends Tree[Nothing]
  * }}}
  *
  * ===Examples of creating instances, and the instances we use throughout the solution===
  * {{{
  *   /*
  *               18
  *            /       \
  *          17         30
  *         /  \        /  \
  *       16    50    100   46
  *      /  \   /
  *     8   7  9
  *  */
  *   val sampleTree: Tree[Int] = ???
  * }}}
  *
  * ===Scala solutions for some of the leetcode problems===
  * {{{
  *   def invertTree[A](t: Tree[A]): Tree[A] = t match {
  *     case EmptyTree => EmptyTree
  *     case Node(entry, left, right) => Node(entry, invertTree(right), invertTree(left))
  *   }
  * }}}
  *
  * ===Invoking aforementioned scala solutions===
  * {{{
  *   println(invertTree(sampleTree))
  * }}}
  *
  *
  */

package object binarytree {}
