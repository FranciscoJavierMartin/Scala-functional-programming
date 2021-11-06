package com.azure.numbers

import scala.annotation.tailrec

object ReverseInteger {

  def main(args: Array[String]): Unit = {
    println("Positives:")
    println(reverseInteger(0))
    println(reverseInteger(9))
    println(reverseInteger(53))
    println(reverseInteger(504))
    println(reverseInteger(540))
    println(reverseInteger(53678534))
    println(reverseInteger(Int.MaxValue))
    println("Negative:")

    println(reverseInteger(-9))
    println(reverseInteger(-53))
    println(reverseInteger(-504))
    println(reverseInteger(-540))
    println(reverseInteger(-53678534))
    println(reverseInteger(Int.MinValue))
  }

  def reverseInteger(number: Int): Int = {
    @tailrec
    def reverseIntegerTailrec(remaining: Int, acc: Int): Int =
      if (remaining == 0) acc
      else {
        val digit = remaining % 10
        val tentativeResult = acc * 10 + digit

        if ((acc >= 0) != (tentativeResult >= 0)) 0
        else reverseIntegerTailrec(remaining / 10, tentativeResult)
      }

    if (number == Int.MinValue) 0
    else if (number >= 0) reverseIntegerTailrec(number, 0)
    else -reverseIntegerTailrec(-number, 0)
  }
}
