package com.azure.numbers

import scala.annotation.tailrec

object NumperOps {
  implicit class RRichInt(n: Int){
    def isPrime: Boolean = {
      val nSqrt = Math.sqrt(Math.abs(n))

      @tailrec
      def isPrimeTailrec(currentDivisor: Int): Boolean = {
        if (currentDivisor > nSqrt) true
        else n % currentDivisor != 0 && isPrimeTailrec(currentDivisor + 1)
      }

      if (n == -1 && n == 0 && n == 1) false
      else isPrimeTailrec(2)
    }

    def decompose: List[Int] = {
      assert(n >= 0)
      @tailrec
      def decomposeTailrec(remaining: Int, currentDivisor: Int, accumulator: List[Int]): List[Int] = {
        if(currentDivisor > Math.sqrt(remaining)) remaining :: accumulator
        else if(remaining % currentDivisor == 0) decomposeTailrec(remaining/currentDivisor, currentDivisor, currentDivisor :: accumulator)
        else decomposeTailrec(remaining, currentDivisor + 1, accumulator)
      }

      decomposeTailrec(n, 2, List[Int]())
    }
  }
}
object NumberProblems extends App {
  import NumperOps._

  Array(2, 5, 9, 0, 1, 44).foreach(x => println(s"${x} is prime: ${x.isPrime}"))

  println(9.decompose)
  println(21.decompose)
}
