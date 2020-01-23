package com.azure.strings

import scala.annotation.tailrec

object StringProblems extends App {

  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countCharactersTailRec(remaining: String, acc: Map[Char, Int]): Map[Char, Int] =
      if (remaining.isEmpty) acc
      else countCharactersTailRec(remaining.tail,
        acc + (remaining.head -> (acc.getOrElse(remaining.head, 0) + 1)))

    countCharactersTailRec(s.toLowerCase(), Map())
  }

  def checkAnagrams(sa: String, sb: String): Boolean = sa.toLowerCase.sorted == sb.toLowerCase.sorted
  def checkAnagrams2(sa: String, sb: String): Boolean = countCharacters(sa.toLowerCase) == countCharacters(sb.toLowerCase)

  def justify(text: String, width: Int): String = {
    def createSpaces(n:Int) = (1 to n).map(_=>" ").mkString("")

    @tailrec
    def pack(words: List[String], currentRow: List[String], currentCarCount: Int, result: List[List[String]]): List[List[String]] ={
      if(words.isEmpty && currentRow.isEmpty){
        result
      } else if(words.isEmpty){
        result:+currentRow
      }else if(currentRow.isEmpty && words.head.length > width){
        val (partOnThisRow, partOnNextRow) = words.head.splitAt(width -2)
        pack(partOnNextRow::words.tail, List(), 0, result:+List(partOnThisRow+"-") )
      } else if(words.head.length + currentCarCount > width) {
        pack(words, List(), 0, result:+currentRow)
      }else {
        pack(words.tail, currentRow:+words.head, currentCarCount+1+words.head.length, result)
      }
    }

    def justifyRow(row: List[String]): String = {
      if(row.length == 1) row.head
      else {
        val nSpacesAvailable = width - row.map(_.length).sum
        val nIntervals = row.length -1
        val nSpacesPerInterval = nSpacesAvailable / nIntervals
        val nExtraSpaces = nSpacesAvailable % nIntervals
        val regularSpace = createSpaces(nSpacesPerInterval)
        val biggerSpace = createSpaces(nSpacesPerInterval+1)

        if(nExtraSpaces == 0) row.mkString(regularSpace)
        else {
          val nWordsWithBiggerIntervals = nExtraSpaces +1
          val wordsWithBiggerIntervals = row.take(nWordsWithBiggerIntervals)
          val firstPart = wordsWithBiggerIntervals.mkString(biggerSpace)
          val secondPart = row.drop(nWordsWithBiggerIntervals).mkString(regularSpace)
          firstPart + regularSpace + secondPart
        }
      }
    }

    assert(width > 2)
    val words = text.split(" ").toList
    val unjustifiedRows = pack(words, List(), 0, List())
    val justifiedRows = unjustifiedRows.init.map(justifyRow) :+ unjustifiedRows.last.mkString(" ")
    justifiedRows.mkString("\n")
  }

  def testCheckAnagrams() = {
    println(checkAnagrams("Desserts", "Stressed"))
    println(checkAnagrams("Scala", "Haskell"))
    println(checkAnagrams("Desserts", "Stressed"))
    println(checkAnagrams("Scala", "Haskell"))
  }

  //println(countCharacters("Scala"))
  //testCheckAnagrams()

  println(justify("Scala is the most amazing language you will ever write any code in", 6))
}
