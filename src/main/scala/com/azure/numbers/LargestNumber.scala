package com.azure.numbers

object LargestNumber {
  def main(args: Array[String]): Unit = {
    println(largestNumber(List(10, 2)))
    println(largestNumber(List(3, 30, 5, 9, 34)))
    println(largestNumber(List(2020, 20, 1010, 10, 2, 22)))
    println(largestNumber(List(1)))
    println(largestNumber(List()))
    println(largestNumber(List(0, 0, 0)))

  }

  def largestNumber(number: List[Int]): String = {
    implicit val newOrdering: Ordering[Int] = Ordering.fromLessThan { (a, b) =>
      val aString = a.toString
      val bString = a.toString
      (aString + bString).compareTo(bString + aString) >= 0
    }

    val largest = number.sorted.mkString("")

    if (number.isEmpty || largest.charAt(0) == '0') "0"
    else largest
  }
}
