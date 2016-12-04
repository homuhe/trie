package com.ir

import java.util.logging.Logger

import scala.collection.SortedSet
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
    // start with root node, i.e. this trie
    insert(word, this)

    def insert(remainingWord: String, node: Node): Unit = {
      if (remainingWord.length > 0) {
        val charIndex = remainingWord.head - ALPHABET_OFFSET
        if (node.nextNode(charIndex) == null)
          node.nextNode(charIndex) = new Node
        insert(remainingWord.tail, node.nextNode(charIndex))
      }
      else node.wordComplete = true  // mark as word after String was completely inserted,
                                    // later required to collect set of existing words
    }
  }

  def searchPrefixNode(prefix: String):  Node = {
    def searchPrefixNode(prefix: String, node: Node): Node = {
      if (prefix.length > 0) {
        val charIndex = prefix.head - ALPHABET_OFFSET

        if(charIndex < 0 || charIndex > 25) return new Node
        if (node.nextNode(charIndex) == null) return new Node // contains method needs a false boolean

        //recursively iterate through subnodes until no character left
        searchPrefixNode(prefix.tail, node.nextNode(charIndex))
      } else node
    }

    searchPrefixNode(prefix, this)  // return the Node where the prefix search ends.
  }

  def containsWord(word: String): Boolean = searchPrefixNode(word).wordComplete

  def searchByPrefix(prefix: String, node: Node): Set[String] ={
    var tempSet = Set[String]()

//    if(node != null)              // holger, bitte testen. ich glaub die brauch ma nicht >>>
      for(charIndex <- node.nextNode.indices){
        if(node.nextNode(charIndex) != null){ // <<< weil wir hier bereits prüfen... und die root node ist nie null, ODER?
          val newWord = prefix + (charIndex + ALPHABET_OFFSET).toChar

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

    showMemoryPerformance()

    def showMemoryPerformance() = {
      // code from: http://alvinalexander.com/scala/how-show-memory-ram-use-scala-application-used-free-total-max
      // memory info
      val mb = 1024*1024
      val runtime = Runtime.getRuntime
      println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
      println("** Free Memory:  " + runtime.freeMemory / mb)
      println("** Total Memory: " + runtime.totalMemory / mb)
      println("** Max Memory:   " + runtime.maxMemory / mb)
    }


    query_call()

    def query(query: String): SortedSet[String] = {

      val asterixAt = query.indexOf("*")
      val suffix = query.substring(asterixAt+1)


      var sortedResults = SortedSet[String]()

      if (query.endsWith("*"))
        prefix_search()
      else if (query.startsWith("*"))
        suffix_search()
      else
        infix_search()

      def prefix_search() = {
        val prefix = query.substring(0, asterixAt)
        trie.searchByPrefix(prefix, trie.searchPrefixNode(prefix))
          .map(element => sortedResults += element)
      }

      def suffix_search() =  { // here we have to do a prefix search on the reversedTrie
      val prefix = suffix.reverse
        reversedtrie
          .searchByPrefix(prefix, reversedtrie.searchPrefixNode(prefix))
          .map(word => word.reverse)
          .map(element => sortedResults += element)
      }

      def infix_search() = { //infix search here
      var prefix = query.substring(0, asterixAt)
        val trieResults = trie.searchByPrefix(prefix, trie.searchPrefixNode(prefix))

        prefix = suffix.reverse

        val reversedTrieResults = reversedtrie
          .searchByPrefix(prefix, reversedtrie.searchPrefixNode(prefix))
          .map(word => word.reverse)

        trieResults.intersect(reversedTrieResults)
          .map(element => sortedResults += element)
      }
      sortedResults
    }

    def query_call(): Unit = {
      print("###Trie-Search: "); val input = StdIn.readLine()
      if (input.contains("*")) {
        println(query(input).size)
        println(query(input))
      }
      else {
        if(trie.containsWord(input)) println(input + " exists in the lexicon.")
        else println(input + " not in lexicon.")
      }
      query_call()
    }
  }
}
