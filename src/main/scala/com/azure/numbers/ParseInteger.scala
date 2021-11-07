package com.azure.numbers

object ParseInteger {
  def parseInteger(string: String): Int = {
    val WHITESPACE = ' '
    val PLUS = '+'
    val MINUS = '-'
    val DIGITS = "0123456789".toSet

    def integerRangeEnd(sign: Int): Int = if (sign >= 0) Int.MaxValue else Int.MinValue

    def parseTailrec(remainder: String, sign: Int, acc: Int = 0): Int =
      if (remainder.isEmpty || !DIGITS.contains(remainder.charAt(0))) acc
      else {
        val newDigit = remainder.charAt(0) - '0'
        val tentativeResult = acc * 10 + newDigit * sign

        if ((sign >= 0) != (tentativeResult >= 0)) integerRangeEnd(sign)
        else parseTailrec(remainder.substring(1), sign, tentativeResult)
      }

    if (string.isEmpty) 0
    else if (string.charAt(0) == WHITESPACE) parseInteger(string.substring(1))
    else if (string.charAt(0) == PLUS) parseTailrec(string.substring(1), sign = 1)
    else if (string.charAt(0) == MINUS) parseTailrec(string.substring(1), sign = -1)
    else parseTailrec(string, sign = 1)
  }

  def main(args: Array[String]): Unit = {
    println(parseInteger(""))
    println(parseInteger("String"))
    println(parseInteger("1"))
    println(parseInteger("-1"))
    println(parseInteger("   Scala"))
    println(parseInteger("   4256"))
    println(parseInteger("   -4256"))
    println(parseInteger("   +4256"))
    println(parseInteger("42 is the meaning of life"))
    println(parseInteger(Int.MaxValue.toString))
    println(parseInteger(Int.MinValue.toString))
  }
}
