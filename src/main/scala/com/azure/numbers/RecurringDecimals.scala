package com.azure.numbers

import scala.annotation.tailrec

object RecurringDecimals extends App {

  def fractionToRecurringDecimals(numerator: Int, denominator: Int): String = {

    def f2d(n: Long, d: Long): String = {

      @tailrec
      def findRecurrenceStart(digit: Long, digits: List[Long], rem: Long, remainders: List[Long], currentIndex: Int): Int = {
        if (digits.isEmpty || remainders.isEmpty) -1
        else if (digit == digits.head && rem == remainders.head) currentIndex
        else findRecurrenceStart(digit, digits.tail, rem, remainders.tail, currentIndex + 1)
      }

      @tailrec
      def fractionDecimalsTailrec(num: Long, denom: Long, digits: List[Long], remainders: List[Long]): String = {
        val quot = (num * 10) / denom
        val rem = (num * 10) % denom

        if (rem == 0) (digits :+ quot).mkString("")
        else {
          val recurrenceStartIndex = findRecurrenceStart(quot, digits, rem, remainders, 0)
          if (recurrenceStartIndex == -1) fractionDecimalsTailrec(rem, denom, digits :+ quot, remainders :+ rem)
          else {
            val (beforeRecurrence, recurrence) = digits.splitAt(recurrenceStartIndex)
            s"${beforeRecurrence.mkString("")}(${recurrence.mkString("")})"
          }
        }
      }

      if (n > 0 && d < 0) s"-${f2d(n, -d)}"
      else if (n < 0 && d > 0) s"-${f2d(-n, d)}"
      else {
        val quotient = n / d
        val remainder = n % d

        if (remainder == 0) s"$quotient"
        else s"$quotient.${fractionDecimalsTailrec(remainder, d, List(), List())}"
      }
    }

    f2d(numerator, denominator)
  }

  println(fractionToRecurringDecimals(1, 3))
  println(fractionToRecurringDecimals(1, 2))
  println(fractionToRecurringDecimals(4, 2))
  println(fractionToRecurringDecimals(1, 6))
  println(fractionToRecurringDecimals(1, 333))
  println(fractionToRecurringDecimals(1, 7))
  println(fractionToRecurringDecimals(1, 2003))
  println(fractionToRecurringDecimals(-1, 2))
  println(fractionToRecurringDecimals(1, Int.MinValue))
}
