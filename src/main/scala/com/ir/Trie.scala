package com.ir

import scala.io.{Source, StdIn}

class Node {
  val nodeArray = Array.fill[Node](26)(null)
  var wordComplete = false
}

class Trie extends Node {

  def add(word: String): Unit = {
    def add(word: String, node: Node): Unit = {
      if (word.length > 0) {

        val index = word.head - 'a'
        if (node.nodeArray(index) == null)
          node.nodeArray(index) = new Node
        add(word.tail, node.nodeArray(index))

      }
      else node.wordComplete = true
    }
    add(word, this)
  }

  def searchPrefixNode(prefix: String):  Node = {
    def searchPrefixNode(prefix: String, node: Node): Node = {
      if (prefix.length > 0) {

        val index = prefix.head - 'a'
        if (node.nodeArray(index) == null)
          return null
        searchPrefixNode(prefix.tail, node.nodeArray(index))
      }
      else node
    }
    searchPrefixNode(prefix, this)
  }

  def containsWord(word: String): Boolean = searchPrefixNode(word).wordComplete

  def searchPrefix(prefix: String, node: Node): Set[String] ={
    var tempSet = Set[String]()

    if(node != null)
      for(charIndex <- node.nodeArray.indices){
        if(node.nodeArray(charIndex) != null){
          val newWord = prefix + (charIndex+97).toChar

          if(node.nodeArray(charIndex).wordComplete)
            tempSet += newWord
          tempSet = tempSet ++ searchPrefix(newWord, node.nodeArray(charIndex))
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
      trie.add(word)
      reversedtrie.add(word.reverse)
    }

    query_call()

    def query(query: String): Set[String] = {

      val asterixAt = query.indexOf("*")
      val suffix = query.substring(asterixAt+1)
      var result = Set[String]()

      if (query.endsWith("*"))
        prefix_search()
      else if (query.startsWith("*"))
        suffix_search()
      else
        infix_search()

      def prefix_search() = {
        val prefix = query.substring(0, asterixAt)
        result = trie.searchPrefix(prefix, trie.searchPrefixNode(prefix))
      }

      def suffix_search() = { // here we have to do a prefix search on the reversedTrie
      val prefix = suffix.reverse
        result = reversedtrie
          .searchPrefix(prefix, reversedtrie.searchPrefixNode(prefix))
          .map(word => word.reverse)
      }

      def infix_search() = { //infix search here
      var prefix = query.substring(0, asterixAt)
        val trieResults = trie.searchPrefix(prefix, trie.searchPrefixNode(prefix))

        prefix = suffix.reverse

        val reversedTrieResults = reversedtrie
          .searchPrefix(prefix, reversedtrie.searchPrefixNode(prefix))
          .map(word => word.reverse)

        result = trieResults.intersect(reversedTrieResults)
      }
      result
    }

    def query_call(): Unit = {
      print("trie-search: "); val input = StdIn.readLine()
      if (input.contains("*")) {
        query(input).foreach(println)
      }
      else {
        //if(trie.containsWord(input)) BUG: aaa throws Exception, why?
        println(input)
      }
      query_call()
    }
  }
}