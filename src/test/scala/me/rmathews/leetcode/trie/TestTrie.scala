package me.rmathews.leetcode.trie

import org.scalatest.FlatSpec

class TestTrie extends FlatSpec {

  "apply" should "populate a trie with words" in {
    val words = List("hello", "world")
    val t = Trie(words)
    println(t)
  }

  "startsWith" should "return true if prefix exists in trie" in {
    val words = List("hello", "world")
    val t = Trie(words)
    assert(t.startsWith("hell"))
  }

  "startsWith" should "return true if whole word exists in trie" in {
    val words = List("hello", "world")
    val t = Trie(words)
    assert(t.startsWith("world"))
  }

  "startsWith" should "return false if prefix does not exist in trie" in {
    val words = List("hello", "world")
    val t = Trie(words)
    assert(!t.startsWith("held"))
  }

  "contains" should "return true if word exists in trie" in {
    val words = List("hello", "world")
    val t = Trie(words)
    assert(t.contains("hello"))
  }

  "contains" should "return false if only prefix exists in trie" in {
    val words = List("hello", "world")
    val t = Trie(words)
    assert(!t.contains("hell"))
  }

  "contains" should "return false if word doesn't exist in trie" in {
    val words = List("hello", "world")
    val t = Trie(words)
    assert(!t.contains("held"))
  }

}
