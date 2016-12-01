package com.ir


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
  val root = new Node

  // magic here
  def addWord(word: String, node: Node): Unit = {


    if (word.length > 0){
//      println("working on: " + word.head + " - remaining: " + word.tail)
      // 'a' has a int value of 97. 'a' - 'a' = 0 -> index
      val index = word.head - 'a'

      if (!node.charArray(index)) {
        node.charArray(index) = true
        node.nodeArray(index) = new Node
      }
      addWord(word.tail, node.nodeArray(index))

      println(node)
    } else println("done!")




  }


  def searchPrefix(word: String): Unit = {}

  def containsWord(word: String): Unit = {}

}


class Node {

  val charArray = Array.fill[Boolean](26)(false)
  val nodeArray = Array.fill[Node](26)(null)

  override def toString = {
    var toString = "| "
    for(charIndex <- 0 until charArray.length-1){
      if(charArray(charIndex))
        toString = toString + (charIndex + 97).toChar
      else
        toString = toString + " "
      toString +=  " | "
    }
    toString.trim
  }
  //\n ${nodeArray.deep.mkString(" ")})"

}

object Trie {
  def main(args : Array[String]): Unit =  {

    val trie = new Trie



    //    trie.addWord("root", trie)
    //    trie.addWord("boot", trie)
    //    trie.addWord("shoot", trie)
//    trie.addWord("za", trie)
    trie.addWord("ab", trie)
    trie.addWord("ab", trie)
//    trie.addWord("ab", trie)

    println("---------------")
    println(trie)
    println(trie.nodeArray(0))

//    println(trie.nodeArray(1))
//    println(trie.nodeArray(0).charArray(1))
//    println(trie.nodeArray(0).nodeArray(1))
    //    println(trie.alphabet.deep.mkString(" "))

    //    println(trie.nodeArray(1))
    //    trie.addWord("a")
    //    trie.addWord("a")
    //    trie.addWord("a")
    //    println(trie.root)


  }
}






