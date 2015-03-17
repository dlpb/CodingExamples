package google.rootedtree

import java.nio.file.Paths

import scala.collection.immutable.HashMap


object RootedTree {


  def removalCount(nodeCount: Int, paths: List[(Int, Int)]): Int = {
    val adjencyMatrix = adjacencyMatrix(paths)
    val tree = tree(nodeCount, paths)
    2
  }

  private def tree(size: Int, paths: List[(Int, Int)]) = {
//    case class EmptyNode() extends Node(null, null, null)
//    case class Node(parent: Node, left: Node, right: Node)
//
//    var nodes = Map[Int, Node] = HashMap()
//    0 until size foreach {
//      i =>
//        nodes
//    }
//    paths foreach {
//
//    }

  }

    def pp(ints: Array[Array[Int]]) = {
    println(ints.deep.mkString("\n"))
  }


  private def adjacencyMatrix(paths: List[(Int, Int)]) = {

    def populateMatrix(matrix: Array[Array[Int]], paths: List[(Int, Int)]): Array[Array[Int]] = {
      val m = matrix
      paths foreach {
        p =>
          m(p._1 -1 )(p._2 -1 ) = 1
      }
      m
    }
    def findRoot(size: Int, matrix: Array[Array[Int]]): Int = {
      var candidateToRemove: Int = 1
      (1 until size) foreach {
        i =>
          if(matrix(candidateToRemove)(i-1) == 1) candidateToRemove = i-1
      }
      var noSink = false
      (1 until size) foreach {
        i =>
          if(candidateToRemove != i && (matrix(candidateToRemove)(i-1) == 1 || matrix(i-1)(candidateToRemove) == 0 ))
            noSink = true
      }
      println(s"Candidate to remove $candidateToRemove, when size is $size")
      candidateToRemove
    }
    def findRemoveCount(root: Int, size: Int, matrix: Array[Array[Int]]) = {
      def findChildren(root: Int, prevRoots: List[Int]): List[Int] = {
        val c1 = (0 until size) map {
          i => if (i != root && !prevRoots.contains(i)) (i, matrix(root)(i)) else (-1, -1)
        } filter(p => p._2 != -1) filter (p => p._2 == 1) map (p => p._1) toList
        val c2 = (0 until size) map {
          i => if (i != root && !prevRoots.contains(i)) (i, matrix(i)(root)) else (-1, -1)
        } filter(p => p._2 != -1) filter (p => p._2 == 1) map (p => p._1) toList
        val list = c1 ++ c2
        println(s"children are $list for root $root")
        list

      }
      def numberOfChildren(root: Int, rootList: List[Int], childCount: Int): Int = {
        val children = findChildren(root, rootList)
        children.size match {
          case 0 =>
            childCount
          case _ =>
            val results: List[Int] = children.map {
              c =>
                numberOfChildren(c, root :: rootList, 1 + childCount )
            }
            results.toList sum
        }

      }

      def prune(root: Int, rootList: List[Int], childCount: Int): Int = {
        val children: List[Int] = findChildren(root, rootList)
        children.size match {
          case 0 =>
            0
          case 1 =>
            numberOfChildren(root, List(), childCount)
          case _ =>
            children.map {
              c: Int =>
                val sum: Int = prune(c, c :: rootList, childCount)
                sum
            }.sum
        }
      }
      val count = prune(0, List(), 0)
      println(s"count is $count")

    }


    val size = paths.size + 1
    var matrix = Array.fill(size, size)(0)
    matrix = populateMatrix(matrix, paths)
    pp(matrix)
    val root: Int = findRoot(size, matrix)
    println(s"root is $root")

    val removeCount = findRemoveCount(root, size, matrix)

  }

}
