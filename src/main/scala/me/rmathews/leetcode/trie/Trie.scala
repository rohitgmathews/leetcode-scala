package me.rmathews.leetcode.trie

import scala.annotation.tailrec

sealed trait Trie {
  /** Returns true if the prefix exists in the trie
    *
    * @param word prefix to check for
    */
  def startsWith(word: String): Boolean

  /** Returns true if the whole word exists in the trie (as opposed to just the prefix)
    *
    * @param word word to check for
    */
  def contains(word: String): Boolean
}

private [trie] case object EmptyTrie extends Trie {
  override def startsWith(word: String): Boolean = false
  override def contains(word: String): Boolean = false
}

private [trie] case class TrieNode(content: Char, children: Array[Trie], var word: Option[String]) extends Trie {
  def startsWith(word: String): Boolean = {
    def go(currNode: Trie, wordIdx: Int): Boolean =
      if (wordIdx == word.length) true
      else currNode match {
        case t @ TrieNode(_, _, _) if t.getChild(word(wordIdx)) != EmptyTrie =>
          go(t.getChild(word(wordIdx)), wordIdx + 1)
        case _ => false
      }
    go(this, 0)
  }
  def contains(word: String): Boolean = {
    def go(currNode: Trie, wordIdx: Int): Boolean =
      if (wordIdx == word.length) currNode match {
        case TrieNode(_, _, Some(`word`)) => true
        case _ => false
      }
      else currNode match {
        case t @ TrieNode(_, _, _) if t.getChild(word(wordIdx)) != EmptyTrie =>
          go(t.getChild(word(wordIdx)), wordIdx + 1)
        case _ => false
      }
    go(this, 0)
  }
  private [trie] def getChild(c: Char): Trie = this.children(c - 'a')
  private [trie] def setChild(c: Char, t: Trie): Unit = this.children(c - 'a') = t
  override def toString: String = s"char: $content, word: $word, children: ${children.filter(_ != EmptyTrie).toList}"
}



object Trie {
  def apply(words: Iterable[String]): Trie = {
    val root = TrieNode(0, Array.fill(26)(EmptyTrie), None)
    @tailrec
    def go(currNode: TrieNode, wordIdx: Int, word: String): Unit =
      if (wordIdx < word.length) currNode.getChild(word(wordIdx)) match {
        case EmptyTrie =>
          val child = TrieNode(word(wordIdx), Array.fill(26)(EmptyTrie), if (wordIdx == word.length - 1) Some(word) else None)
          currNode.setChild(word(wordIdx), child)
          go(child, wordIdx + 1, word)
        case t @ TrieNode(_, _, _) =>
          if (wordIdx == word.length - 1) t.word = Some(word)
          else go(t, wordIdx + 1, word)
      }
    words foreach { word => go(root, 0, word) }
    root
  }
}

