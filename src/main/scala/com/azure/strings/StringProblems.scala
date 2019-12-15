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

  println(countCharacters("Scala"))
}
