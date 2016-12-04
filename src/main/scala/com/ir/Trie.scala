package com.ir

import scala.collection.immutable.SortedSet
import scala.io.{Source, StdIn}

class Node {
  val nextNode = new Array[Node](26)
  var wordComplete = false
}

class Trie extends Node {

  val ALPHABET_OFFSET = 97 // the lower case alphabet begins at 97
  // 'a' has an int value of 97, respectively its position in the ASCII table
  // for the Node class we make use of the numbers 0-25 as indices corresponding to the alphabet letters

  def insert(word: String): Unit = {
    def insert(word: String, node: Node): Unit = {
      if (word.length > 0) {

        val charIndex = word.head - ALPHABET_OFFSET
        if (node.nextNode(charIndex) == null)
          node.nextNode(charIndex) = new Node
        insert(word.tail, node.nextNode(charIndex))
      }
      else node.wordComplete = true  // mark as word after String was completely inserted,
      // later required to collect set of existing words
    }
    // start with root node, i.e. this trie
    insert(word, this)
  }

  def searchPrefixNode(prefix: String):  Node = {
    def searchPrefixNode(prefix: String, node: Node): Node = {
      if (prefix.length > 0) {

        val index = prefix.head - 'a'
        if (node.nextNode(index) == null)
          return new Node
        searchPrefixNode(prefix.tail, node.nextNode(index))
      }
      else node
    }
    searchPrefixNode(prefix, this)
  }

  def contains(word: String): Boolean = searchPrefixNode(word).wordComplete

  def searchByPrefix(prefix: String, node: Node): SortedSet[String] = {
    var tempSet = SortedSet[String]()

    for(charIndex <- node.nextNode.indices) {
      if(node.nextNode(charIndex) != null) {
        val newWord = prefix + (charIndex+97).toChar

        if(node.nextNode(charIndex).wordComplete)
          tempSet += newWord
        tempSet = tempSet ++ searchByPrefix(newWord, node.nextNode(charIndex))
      }
    }
    tempSet
  }
}

object Trie {

  def main(args : Array[String]): Unit =  {

    val trie = new Trie
    val reversedtrie = new Trie

    val lines = Source.fromFile("sowpods.txt").getLines()

    for (word <- lines) {
      trie.insert(word)
      reversedtrie.insert(word.reverse)
    }

    query_call()

    def query(query: String): SortedSet[String] = {

      val asterixAt = query.indexOf("*")
      val suffix = query.substring(asterixAt+1)
      var result = SortedSet[String]()

      if (query.endsWith("*"))
        prefix_search()
      else if (query.startsWith("*"))
        suffix_search()
      else
        infix_search()

      def prefix_search() = {
        val prefix = query.substring(0, asterixAt)
        result = trie.searchByPrefix(prefix, trie.searchPrefixNode(prefix))
      }

      def suffix_search() = { // here we have to do a prefix search on the reversedTrie
        val prefix = suffix.reverse
        result = reversedtrie
          .searchByPrefix(prefix, reversedtrie.searchPrefixNode(prefix))
          .map(word => word.reverse)
      }

      def infix_search() = { //infix search here
        var prefix = query.substring(0, asterixAt)
        val trieResults = trie.searchByPrefix(prefix, trie.searchPrefixNode(prefix))

        prefix = suffix.reverse

        val reversedTrieResults = reversedtrie
          .searchByPrefix(prefix, reversedtrie.searchPrefixNode(prefix))
          .map(word => word.reverse)

        result = trieResults.intersect(reversedTrieResults)
      }
      result
    }

    def query_call(): Unit = {
      print("trie-search: "); val input = StdIn.readLine().toLowerCase
      if (input.count(_ == '*') == 1) {
        if (trie.contains(input.filter(_ != '*'))) //empty '*' case
          println(input.filter(_ != '*'))
        query(input).foreach(println)
      }
      else if (input.count(_ == '*') == 0) {
        if(trie.contains(input)) println(input)
        else println(input + " not in lexicon.")
      }
      else println("Only 1 '*' or less allowed in search query.")
      query_call()
    }
  }
}