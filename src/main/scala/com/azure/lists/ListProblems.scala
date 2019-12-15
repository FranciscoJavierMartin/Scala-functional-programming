package com.azure.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]

  def ++[S>:T](anotherList:RList[S]): RList[S]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  override def apply(index: Int): T = {
    @tailrec
    def applyTailrec(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyTailrec(remaining.tail, currentIndex + 1)
    }

    if(index <0) throw new NoSuchElementException
    else applyTailrec(this, 0)
  }

  override def length: Int = {
    @tailrec
    def lengthTailrec(remainingList: RList[T], accumulator: Int): Int = {
      if(remainingList.isEmpty) accumulator
      else lengthTailrec(remainingList.tail, accumulator +1 )
    }

    lengthTailrec(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseTailrec(remainingList: RList[T], accumulator:RList[T]): RList[T] ={
      if(remainingList.isEmpty) accumulator
      else reverseTailrec(remainingList.tail, remainingList.head :: accumulator)
    }

    reverseTailrec(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatTailrec(remainingList: RList[S], accumulator: RList[S]): RList[S] =
      if(remainingList.isEmpty) accumulator
      else concatTailrec(remainingList.tail, remainingList.head :: accumulator)

    concatTailrec(anotherList, this.reverse).reverse
  }
}

object RList{
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def convertToRListTailrec(remaining: Iterable[T], accumulator: RList[T]): RList[T] = {
      if (remaining.isEmpty) accumulator
      else convertToRListTailrec(remaining.tail, iterable.head :: accumulator)
    }

    convertToRListTailrec(iterable, RNil).reverse
  }
}

object ListProblems extends App {
  //(2) :: RNil == RNil.::(2)
  val aSmallList = 1 :: 2 :: 3 :: RNil
  println(aSmallList)
  //println(aSmallList.apply(0))
  //println(aSmallList.apply(2))
  //println(aSmallList.apply(90))
  //println(aSmallList.length)
  println(aSmallList.reverse)
  println(aSmallList++aSmallList)
}
