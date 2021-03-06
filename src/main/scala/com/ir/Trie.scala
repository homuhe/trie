package com.ir

import scala.collection.immutable.SortedSet
import scala.io.{Source, StdIn}

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  *
  * Task:         Assignment 2
  * Description:  Wildcard query search as Trie implementation.
  */


/**
  * Node class consisting of: - Node Array of size 26
  *                           - boolean which marks if Node contains a complete word
  */
class Node {
  val nextNode = new Array[Node](26)
  var wordComplete = false
}

/**
  * Trie class consisting of Nodes
  */
class Trie extends Node {

  val ALPHABET_OFFSET = 97 // Lowercase alphabet begins at 97.
  // 'a' has an int value of 97, respectively its position in ASCII table.
  // For the Node class we make use of numbers 0-25 as indices, corresponding to the alphabet letters.

  /**
    * Wrapper method which calls recursive insert method on current node.
    * @param word   String which will be inserted into Trie
    *
    */
  def insert(word: String): Unit = {

    def insert(word: String, node: Node): Unit = {
      if (word.length > 0) {

        val charIndex = word.head - ALPHABET_OFFSET
        if (node.nextNode(charIndex) == null)
          node.nextNode(charIndex) = new Node
        insert(word.tail, node.nextNode(charIndex))
      }
      else node.wordComplete = true
    }

    insert(word, this)
  }

  /**
    * Wrapper method which calls recursive searchPrefixNode method on current node.
    * @param prefix   String which will be traversed until last node is reached
    * @return         Last Node of 'prefix'
    */
  def searchPrefixNode(prefix: String):  Node = {

    def searchPrefixNode(prefix: String, node: Node): Node = {
      if (prefix.length > 0) {

        val index = prefix.head - ALPHABET_OFFSET
        if (node.nextNode(index) == null)
          return new Node
        searchPrefixNode(prefix.tail, node.nextNode(index))
      }
      else node
    }

    searchPrefixNode(prefix, this)
  }

  /**
    * Checks if Trie contains argument.
    * @param word   String which will be traversed & whose last Node has to contain a complete word.
    * @return       Boolean value of last Node of 'word' in Trie
    */
  def contains(word: String): Boolean = searchPrefixNode(word).wordComplete

  /**
    * Searches all possible complete words of 'prefix' in Trie.
    * @param prefix   String whose last Node will be traversed to all possible words
    * @param node     Last Node of 'prefix'
    * @return         Sorted Set of all complete words of 'prefix'
    */
  def searchByPrefix(prefix: String, node: Node): SortedSet[String] = {
    var tempSet = SortedSet[String]()
    if(node.wordComplete) tempSet += prefix    // adding the newly created word to the Set

    for(charIndex <- node.nextNode.indices) {
      if(node.nextNode(charIndex) != null) {
        val newWord = prefix + (charIndex+ALPHABET_OFFSET).toChar     //adding next possible letter to prefix
        tempSet = tempSet ++ searchByPrefix(newWord, node.nextNode(charIndex))  // gather rest recursively
      }
    }
    tempSet
  }
}

object Trie {

  /**
    * Creates Trie & reversed Trie based on Source. Handles query call.
    * @param args filename of a text file containing a word list
    */
  def main(args : Array[String]): Unit =  {

    var file = ""

    if (args.length != 1) help()
    else {
      file = args(0)
    }

    val trie = new Trie
    val reversedtrie = new Trie

    val lines = Source.fromFile(file).getLines()

    for (word <- lines) {
      trie.insert(word)
      reversedtrie.insert(word.reverse)
    }

    println("#################|TRIE-SEARCHER|#################")
    query_call()

    /**
      * Handles query call by '*' position in input & calls prefix/suffix/infix_search respectively.
      * @param query  Normalized user input
      * @return       Sorted Set of query results
      */
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

      /**
        * queries which end with '*'
        */
      def prefix_search() = {
        val prefix = query.substring(0, asterixAt)
        result = trie.searchByPrefix(prefix, trie.searchPrefixNode(prefix))
      }

      /**
        * queries which start with '*'
        */
      def suffix_search() = {
        val prefix = suffix.reverse
        result = reversedtrie
          .searchByPrefix(prefix, reversedtrie.searchPrefixNode(prefix))
          .map(word => word.reverse)
      }

      /**
        * queries which contain '*' but don't start or end with it.
        * Calls 'prefix_search' on query and 'suffix_search' on inverted query, returns intersection.
        */
      def infix_search() = {
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

    /**
      * Handles (possibly invalid) user inputs.
      * Only ([A-Z][a-z])*('*')?([A-Z][a-z])* will be searched,
      *                                       uppercase will be normalized,
      *                                       other inputs will be rejected.
      */
    def query_call(): Unit = {

      print("\nEnter your query: "); val input = StdIn.readLine().toLowerCase

      // Only only [a-z]* alphabet (/w asterix) queries allowed
      if (input.matches("([a-z]*|\\*)*")) {
        if (input.count(_ == '*') == 1) {
          val storeResults = query(input)
          if(storeResults.nonEmpty) storeResults.foreach(println)
          else println("No findings matching your query.")
        }
        else if (input.count(_ == '*') == 0) {
          if(trie.contains(input)) println(input + " exists in the lexicon.")
          else println(input + " not in lexicon.")
        }
        else println("Only one or less '*' symbols are allowed.")
      }
      else println("Only letters A-Za-z are allowed.")
      query_call()
    }
  }

  /**
    * Help function for correct usage
    */
  def help() = {
    println("Usage: ./wildcard arg1")
    println("\t\targ1: INPUT - filename of a text file containing a word list")
    sys.exit()
  }
}