package com.azure.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean
  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int
  // The number of nodes in the tree
  val size: Int
  def collectNodes(level: Int): List[BTree[T]]
  def mirror: BTree[T]
  def sameShapeAs[S>:T](that: BTree[S]): Boolean
  def isSymmetrical: Boolean
}

case object BEnd extends BTree[Nothing]{
  override def value: Nothing = throw new NoSuchElementException
  override def left: BTree[Nothing] = throw new NoSuchElementException
  override def right: BTree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false
  override def collectLeaves: List[BTree[Nothing]] = List()
  override def leafCount: Int = 0
  override val size: Int = 0

  override def collectNodes(level: Int): List[BTree[Nothing]] = List()
  override def mirror: BTree[Nothing] = BEnd
  override def sameShapeAs[S >: Nothing](that: BTree[S]): Boolean = that.isEmpty
  override def isSymmetrical: Boolean = true
}

case class BNode[+T](override val value: T, override val left:BTree[T], override val right: BTree[T]) extends BTree[T]{
  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {
    @tailrec
    def collectLeavesTailrec(todo: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] = {
      if(todo.isEmpty) leaves
      else if(todo.head.isEmpty) collectLeavesTailrec(todo.tail, leaves)
      else if(todo.head.isLeaf) collectLeavesTailrec(todo.tail, todo.head :: leaves)
      else {
        val node = todo.head
        collectLeavesTailrec(node.left :: node.right :: todo.tail, leaves)
      }
    }

    collectLeavesTailrec(List(this), List())
  }

  override def leafCount: Int = collectLeaves.length

  override val size: Int = 1 + left.size + right.size

  override def collectNodes(level: Int): List[BTree[T]] = {
    @tailrec
    def collectNodesTailrec(currentLevel:  Int, currentNodes: List[BTree[T]]): List[BTree[T]] = {

    }

    if(level < 0) List()
    else collectNodesTailrec(0, List(this))
  }

  override def mirror: BTree[T] = ???

  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = ???

  override def isSymmetrical: Boolean = ???
}

object BinaryTreeProblems extends App {

  val tree = BNode(1,
    BNode(2,
      BNode(3, BEnd, BEnd),
      BNode(4,
        BEnd,
        BNode(5, BEnd, BEnd),
      )
    ),
    BNode(6,
      BNode(7, BEnd, BEnd),
      BNode(8, BEnd, BEnd)
    )
  )

  println(tree.collectLeaves.map(_.value))
  println(tree.leafCount)

  val degenerate = (1 to 100000).foldLeft[BTree[Int]](BEnd)((tree, number) => BNode(number, tree, BEnd))
  println(degenerate.size)
}
