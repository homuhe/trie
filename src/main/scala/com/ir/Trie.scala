package com.ir

import scala.io.{Source, StdIn}


class TrieSearcher{

  val trie = new Trie
  val reversedTrie = new Trie
}

class Trie extends Node{

  // final alphabet
  //TODO DELETE
  var alphabet: Array[Char] = Array()
  for(index <- 0 to 25){
    alphabet = alphabet :+ (index+97).toChar
  }

  // dummy constructor to only take input word an not the trie not as input
  def insertWord(word: String): Unit ={
    def insertWord(word: String, node: Node): Unit = {
      if (word.length > 0){

        val index = word.head - 'a'

        if (node.nodeArray(index) == null)
          node.nodeArray(index) = new Node
        insertWord(word.tail, node.nodeArray(index))

      } else node.wordComplete = true
    }
    insertWord(word, this)
  }

  def findAllWordsWithPrefix(prefix: String, node: Node): Set[String] ={
    var tempSet = Set[String]()

    if(node != null)
      for(charIndex <- node.nodeArray.indices){
        if(node.nodeArray(charIndex) != null){
          val newWord = prefix + (charIndex+97).toChar

          if(node.nodeArray(charIndex).wordComplete)
            tempSet += newWord
          tempSet = tempSet ++ findAllWordsWithPrefix(newWord, node.nodeArray(charIndex))
        }
      }
    tempSet
  }

  def containsWord(word: String): Boolean = searchPrefixNode(word).wordComplete

  def searchPrefixNode(prefix: String):  Node = {
    def searchPrefixNode(prefix: String, node: Node): Node = {
      if (prefix.length > 0){
        val index = prefix.head - 'a'
        if (node.nodeArray(index) == null) return null
        searchPrefixNode(prefix.tail, node.nodeArray(index))
      } else node
    }
    searchPrefixNode(prefix, this)
  }

}


class Node {
  val nodeArray = Array.fill[Node](26)(null)
  var wordComplete = false
}

object Trie {
  def main(args : Array[String]): Unit =  {

    val trie = new Trie
    val reversedtrie = new Trie

    val lines = Source.fromFile("sowpods.txt").getLines()

    lines.foreach(line => trie.insertWord(line))
    lines.foreach(line => reversedtrie.insertWord(line.reverse))


    def query(query: String): Unit ={

      if(query.contains("*")) {
        val asterixAt = query.indexOf("*")
        var prefix = ""
        val suffix = query.substring(asterixAt+1)

        var trieResults = Set[String]()
        var reversedTrieResults = Set[String]()
        if(query.endsWith("*")) { //normal search with a given prefix
          prefix = query.substring(0, asterixAt)
          trieResults = trie.findAllWordsWithPrefix(prefix, trie.searchPrefixNode(prefix))
          println(trieResults)
        } else if(query.startsWith("*")) { // here we have to do a prefix search on the reversedTrie
          prefix = suffix.reverse
          reversedTrieResults = reversedtrie.findAllWordsWithPrefix(prefix, reversedtrie.searchPrefixNode(prefix))
            .map(word => word.reverse)
          println(reversedTrieResults)
        } else { //infix search here
          prefix = query.substring(0, asterixAt)
          trieResults = trie.findAllWordsWithPrefix(prefix, trie.searchPrefixNode(prefix))

          prefix = suffix.reverse
          reversedTrieResults = reversedtrie.findAllWordsWithPrefix(prefix, reversedtrie.searchPrefixNode(prefix))
            .map(word => word.reverse)

          println(trieResults.intersect(reversedTrieResults))

        }
      } else{ // else we have a normal contains word search
        if(trie.containsWord(query)) println(query + " is a word.")
        else println(query + " does not exist in the lexicon.")
      }
    }

    def input: String = StdIn.readLine()
    while(true){
      println("your search: ")
      query(input)
    }

  }
}