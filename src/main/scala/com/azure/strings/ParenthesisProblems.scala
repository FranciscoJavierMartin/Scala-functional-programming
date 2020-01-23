package com.azure.strings

import scala.annotation.tailrec

object ParenthesisProblems extends App {

  def hasValidParentheses(string: String): Boolean = {
    @tailrec
    def hasValidParenthesesTailrec(remaining: String, openParens: Int): Boolean = {
      if(remaining.isEmpty) openParens == 0
      else if(openParens == 0 && remaining.head == ')') false
      else if(remaining.head == '(') hasValidParenthesesTailrec(remaining.tail, openParens + 1)
      else hasValidParenthesesTailrec(remaining.tail, openParens - 1)
    }

    hasValidParenthesesTailrec(string, 0)
  }

  def generateAllValidParentheses(n:Int): List[String] = {
    @tailrec
    def genParensTailrec(nRemainingParens: Int, currentStrings: Set[String]): Set[String] = {
      if(nRemainingParens == 0) currentStrings
      else {
        val newStrings = for {
          string <- currentStrings
          index <- 0 until string.length
        } yield {
          val (before, after) = string.splitAt(index)
          s"$before()$after"
        }

        genParensTailrec(nRemainingParens -1, newStrings)
      }
    }

    assert( n>=0)

    if(n==0)List()
    else genParensTailrec(n-1, Set("()")).toList
  }

  def testValidParentheses() = {
    println(hasValidParentheses("()"))
    println(hasValidParentheses(")("))
    println(hasValidParentheses("()()"))
    println(hasValidParentheses("(())"))
    println(hasValidParentheses("())"))
    println(hasValidParentheses(")()"))
    println(hasValidParentheses("(()()(()))((((())())(())))"))
  }

  def testGenParens() = {
    println(generateAllValidParentheses(1))
    println(generateAllValidParentheses(2))
    println(generateAllValidParentheses(3))
    println(generateAllValidParentheses(10))
  }

  //testValidParentheses()
  testGenParens()
}
