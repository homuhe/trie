package com.ir

import java.util
import java.util.{Collections, Comparator}

/**
  * @author ${user.name}
  */
object Trie {


  def main(args : Array[String]): Unit =  {

    val trie = new Trie




//    println(trie._start.getOrCreate('a'))


//    trie._start.getOrCreate('a')
//    trie._start.getOrCreate('b')
//    trie._start.children.foreach(_.node.getOrCreate('u'))




    trie.addWord("ABBA")
    println(trie.getEgdes)
  }

}


trait trie{
  def addWord(word: String)
  def searchPrefix(prefix: String)
}



class Trie extends Node with trie{

  //  private[this] var _edges: List[Edge] = List[Edge]()
  var root: Node = Node()


  def getEgdes: List[Edge] = root.children





  def addWord(word: String): Unit = {
    val charSequence: Array[Char] = word.toCharArray

        for (char <- charSequence) {
          root.getOrCreate(char)
        }

//        if(char)
  }

  def searchPrefix(word: String) = {
    ""
  }

  //  class EdgeComparator extends Comparator[Edge]{
  //    override def compare(edge1: Edge, edge2: Edge): Int = Character.compare(edge1.ch, edge2.ch)
  //  }

}

case class Edge(char: Char, to: Node){
  val ch = char
  val node = to
  //    override def toString = s"Edge($ch, $to)"
}

case class Node() {
  var children: List[Edge] = Nil

  def getOrCreate(char: Char): List[Edge] = {
    val edge: Edge = Edge(char, Node())
    //    val idx: Int = edges.
    //        Collections.binarySearch(edges, new Edge(char, null), new EdgeComparator)


    if (!this.children.contains(edge)) {
      children =  children :+ edge
    } else {
      for(child <- children){
        child.node.children = child.node.children :+ edge
      }

      //        children

      //        println(edges.indexOf(edges)
    }
    children
  }

  override def toString: String = s"$children"
}






