package lectures.part1basics

import scala.annotation.tailrec

object Recursion extends App {


  def concatenate(s: String, times: Int): String = {
      @tailrec
      def helper(acc: String, n: Int): String =
        if (n <= 0) acc
        else helper(s + acc, n - 1)

      helper("", times)
  }
  println(concatenate("Hello", 5))

  def isPrime(number: Int): Boolean = {
    @tailrec
    def isPrimeTailRec(index: Int): Boolean = {
      if (index <= 1) true
      else if (number % index == 0) false
      else isPrimeTailRec(index - 1)
    }

    if (number <= 1) false
    else isPrimeTailRec(number/2)
  }
  println(isPrime(629))

  def fibo(number: Int): Int = {
    @tailrec
    def fiboTailRec(index: Int, prev: Int, curr: Int): Int = {
      if (index <= 0) curr
      else fiboTailRec(index - 1, prev + curr, prev)
    }

    fiboTailRec(number, 1, 0)
  }
  println(fibo(6))
}
