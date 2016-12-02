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

        if (node.nodeArray(index) == null) {
          node.nodeArray(index) = new Node
        }
        addWord(word.tail, node.nodeArray(index))

        //      println(node)
      } else node.wordComplete = true
    }
  }





  def searchPrefix(word: String): Unit = {}
  def findAllWordsWithPrefix(word: String): Unit ={}

  def asterixSearch(query: String): Unit ={
    if(query.startsWith("*")) {}
      //suffix search - prefixsearch of reversed trie
    if(query.endsWith("*")) {} //prefix search - prexisearch of normal trie

    //otherwise infix search
    val asterixAt = query.indexOf("*")
    val prefix = query.substring(0, asterixAt-1)
    val suffix = query.substring(asterixAt+1)
  }

  def containsWord(word: String): Boolean = {

    var charIndices = List[Int]()
    for(char <- word){
      val charIndex = (char-97)
      if(charIndex > 25 || charIndex < 0)
        return false
      charIndices = charIndices :+ charIndex
    }
    searchWord(charIndices, this)
  }

  def searchWord(indices: List[Int], node: Node): Boolean = {
    if(node == null) return false
    if(indices.isEmpty){
      node.wordComplete
    }else{
      if(node.nodeArray == null)
        return false
      if(node.nodeArray(indices.head) == null)
        return false
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

    println(trie.containsWord("gunmen"))
    println(trie.containsWord("gunmennen"))
    //großbuchstabe
    println(trie.containsWord("Behinderung"))

    println("---")
    println(trie.containsWord("jew"))
    println(trie.containsWord("nazi"))
    println(trie.containsWord("hitler"))

    println("~~~")
    //taste rückwärts
    println(trie.containsWord("taste"))
    println(trie.containsWord("etsat"))
    println(reversedtrie.containsWord("etsat"))

    println("Es ist vollbracht!")


  }
}






