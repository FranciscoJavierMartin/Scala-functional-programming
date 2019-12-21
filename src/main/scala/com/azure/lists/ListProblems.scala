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

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]

  def map[S](F: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  // run-length encoding
  def rle: RList[(T, Int)]
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

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](F: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  override def rle: RList[(Nothing, Int)] = RNil
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

    if (index < 0) throw new NoSuchElementException
    else applyTailrec(this, 0)
  }

  override def length: Int = {
    @tailrec
    def lengthTailrec(remainingList: RList[T], accumulator: Int): Int = {
      if (remainingList.isEmpty) accumulator
      else lengthTailrec(remainingList.tail, accumulator + 1)
    }

    lengthTailrec(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseTailrec(remainingList: RList[T], accumulator: RList[T]): RList[T] = {
      if (remainingList.isEmpty) accumulator
      else reverseTailrec(remainingList.tail, remainingList.head :: accumulator)
    }

    reverseTailrec(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatTailrec(remainingList: RList[S], accumulator: RList[S]): RList[S] =
      if (remainingList.isEmpty) accumulator
      else concatTailrec(remainingList.tail, remainingList.head :: accumulator)

    concatTailrec(anotherList, this.reverse).reverse
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtTailrec(remainingList: RList[T], predecessor: RList[T], currentIndex: Int): RList[T] =
      if (currentIndex == index) predecessor.reverse ++ remainingList.tail
      else if (remainingList.isEmpty) predecessor.reverse
      else removeAtTailrec(remainingList.tail, remainingList.head :: predecessor, currentIndex + 1)

    if (index < 0) this
    else removeAtTailrec(this, RNil, 0)
  }

  override def map[S](F: T => S): RList[S] = {
    @tailrec
    def mapTailrec(remainingList: RList[T], accumulator: RList[S]): RList[S] =
      if(remainingList.isEmpty) accumulator.reverse
      else mapTailrec(remainingList.tail, F(remainingList.head) :: accumulator)

    mapTailrec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {

    /*
      Complexity? O(Z^2)
    */
    @tailrec
    def flatMapTailrec(remainingList: RList[T], accumulator: RList[S]): RList[S] = {
      if(remainingList.isEmpty) accumulator.reverse
      else flatMapTailrec(remainingList.tail, f(remainingList.head).reverse ++ accumulator)
    }

    /*
      Complexity: O(N + Z)
    */
    @tailrec
    def betterFlatMap(remainingList: RList[T], accumulator: RList[RList[S]]): RList[S] = {
      if(remainingList.isEmpty) concatenateAll(accumulator, RNil, RNil)
      else betterFlatMap(remainingList.tail, f(remainingList.head).reverse :: accumulator)
    }

    /*
      Complexity: O(Z)
     */
    @tailrec
    def concatenateAll(elements: RList[RList[S]], currentList: RList[S], accumulator: RList[S]): RList[S] = {
      if(currentList.isEmpty && elements.isEmpty) accumulator
      else if(currentList.isEmpty) concatenateAll(elements.tail, elements.head, accumulator)
      else concatenateAll(elements, currentList.tail, currentList.head :: accumulator)
    }

    betterFlatMap(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterTailrec(remainingList: RList[T], accumulator: RList[T]): RList[T] =
      if(remainingList.isEmpty) accumulator
      else if (f(remainingList.head)) filterTailrec(remainingList.tail, head :: accumulator)
      else filterTailrec(remainingList.tail, accumulator)


    filterTailrec(this, RNil)
  }

  override def rle: RList[(T, Int)] = {
    @tailrec
    def rleTailrec(remainingList: RList[T], currentTuple: (T, Int), accumulator: RList[(T, Int)]): RList[(T, Int)] = {
      if(remainingList.isEmpty && currentTuple._2 == 0) accumulator
      else if (remainingList.isEmpty) currentTuple :: accumulator
      else if (remainingList.head == currentTuple._1) rleTailrec(remainingList.tail, currentTuple.copy(_2 = currentTuple._2 + 1), accumulator)
      else rleTailrec(remainingList.tail, (remainingList.head, 1), currentTuple :: accumulator)
    }

    rleTailrec(this.tail, (this.head, 1), RNil).reverse
  }
}

object RList {
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
  val anotherSmallList = 1 :: 1 :: 2 :: 2 :: 3 :: RNil
  println(aSmallList)
  //println(aSmallList.apply(0))
  //println(aSmallList.apply(2))
  //println(aSmallList.apply(90))
  //println(aSmallList.length)
  //println(aSmallList.reverse)
  //println(aSmallList ++ aSmallList)
  //println(aSmallList.removeAt(2))
  println(anotherSmallList.rle)
}
