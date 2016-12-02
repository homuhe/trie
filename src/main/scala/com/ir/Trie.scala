package com.ir

import scala.io.Source


/**
  * @author ${user.name}
  */

class Trie extends Node{

  // final alphabet
  var alphabet: Array[Char] = Array()
  for(index <- 0 to 25){
    alphabet = alphabet :+ (index+97).toChar
  }
  //intial start node : root
  //  val root = new Node

  // dummy constructor to only take input word an not the trie not as input
  def addWord(word: String): Unit ={
    addWord(word, this)

    def addWord(word: String, node: Node): Unit = {
      if (word.length > 0){
        // 'a' has a int value of 97. 'a' - 'a' = 0 -> index
        val index = word.head - 'a'

        if (node.nodeArray(index) == null)
          node.nodeArray(index) = new Node
        addWord(word.tail, node.nodeArray(index))

      } else node.wordComplete = true
    }
  }





  def prefixSearch(query: String): Set[String] = {
    val asterixAt = query.indexOf("*")
    val prefix = query.substring(0, asterixAt)
    val suffix = query.substring(asterixAt+1)

    println("asterixAt: " + asterixAt)
    println(prefix + " " + suffix)



    var output = Set[String]()
    //prefix search - prexisearch of normal trie
    if(query.endsWith("*")) {
      output = findAllWordsWithPrefix(prefix, commonPrefixNode(prefix))
    }
    //suffix search - prefixsearch of reversed trie
    if(query.startsWith("*")) {
      output = findAllWordsWithPrefix(prefix, commonPrefixNode(prefix))
    }
    //otherwise infix search
    // intersect of both
    output

  }

  def commonPrefixNode(prefix: String):  Node = searchWord(createCharIndices(prefix), this)


  def findAllWordsWithPrefix(prefix: String, node: Node): Set[String] ={
    var tempSet = Set[String]()

    for(charIndex <- 0 until node.nodeArray.length){
      if(node.nodeArray(charIndex) != null){
        val newWord = (prefix + alphabet(charIndex))

        if(node.nodeArray(charIndex).wordComplete){
          tempSet += newWord
        }
        tempSet = tempSet ++ findAllWordsWithPrefix(newWord, node.nodeArray(charIndex))
      }
    }
    tempSet
  }

  def containsWord(word: String): Boolean = {
    return searchWord(createCharIndices(word), this).wordComplete
  }

  def createCharIndices(word: String): List[Int] = {
    var charIndices = List[Int]()
    for(char <- word){
      val charIndex = (char-97)
      if(charIndex > 25 || charIndex < 0)
        return List[Int]()
      charIndices = charIndices :+ charIndex
    }
    charIndices
  }

  def searchWord(indices: List[Int], node: Node): Node = {
    val dummyNode = new Node
    if(node == null) return dummyNode
    if(indices.isEmpty){
      node
    }else{
      if(node.nodeArray == null)
        return dummyNode
      if(node.nodeArray(indices.head) == null)
        return dummyNode
      searchWord(indices.drop(1), node.nodeArray(indices.head))
    }
  }



}


class Node {
  //  val charArray = Array.fill[Boolean](26)(false)
  val nodeArray = Array.fill[Node](26)(null)
  var wordComplete = false

  override def toString = {
    var toString = "| "
    for(charIndex <- 0 until nodeArray.length){
      if(nodeArray(charIndex) != null)
        toString = toString + (charIndex + 97).toChar
      else
        toString = toString + " "
      toString +=  " | "
    }
    toString.trim
  }
}

object Trie {
  def main(args : Array[String]): Unit =  {

    val trie = new Trie
    val reversedtrie = new Trie


    var lines = Source.fromFile("sowpods.txt").getLines()

    for(line <- lines){
      //      if(!line.matches("[a-z]*"))
      //      println(line)
      trie.addWord(line)
      reversedtrie.addWord(line.reverse)
    }

    for(line <-lines){
      trie.containsWord(line)
    }

    //    println(trie.containsWord("gunmen"))
    //    println(trie.containsWord("gunmennen"))
    //    //großbuchstabe
    //    println(trie.containsWord("Behinderung"))
    //
    //    println("---")
    //    println(trie.containsWord("jew"))
    //    println(trie.containsWord("nazi"))
    //    println(trie.containsWord("hitler"))
    //
    //    println("~~~")
    //    //taste rückwärts
    //    println(trie.containsWord("taste"))
    //    println(trie.containsWord("etsat"))
    //    println(reversedtrie.containsWord("etsat"))
    //
    //    println("Es ist vollbracht!")


    //    trie.addWord("gun")
    //    trie.addWord("guns")
    //    trie.addWord("gunmen")
    //    trie.addWord("gunner")


    println(trie.prefixSearch("gun*"))
    println(trie.prefixSearch("gun*er"))
//    println(trie.prefixSearch("haha"))
//    println(trie.prefixSearch("mongo"))
//    println(trie.prefixSearch("jew"))
    //    println(trie.findAllWordsWithPrefix("gun", trie.commonPrefixNode("gun")))


  }
}






