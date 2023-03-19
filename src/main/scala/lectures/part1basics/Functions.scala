package lectures.part1basics

import scala.annotation.tailrec

object Functions extends App {

  def greet(name: String, age: Int): String = "Hi, my name is " + name + " and I am " + age + " years old"

  def factorial(number: Int) = {
    @tailrec
    def factorialTailRec(numberRec: Int, acc: Int): Int = {
      if (numberRec == 1) numberRec * acc
      else factorialTailRec(numberRec - 1, numberRec * acc)
    }

    if (number <= 0) 1
    else factorialTailRec(number, 1)
  }

  def fibo(number: Int): Int = {
    @tailrec
    def fiboTailRec(index: Int, prev: Int, curr: Int): Int = {
      if (index <= 0) curr
      else fiboTailRec(index - 1, prev + curr, prev)
    }
    fiboTailRec(number, 1, 0)
  }

  def isPrime(number: Int): Boolean = {
    @tailrec
    def isPrimeTailRec(index: Int): Boolean = {
      if (index <= 1) true
      else if (number % index == 0) false
      else isPrimeTailRec(index - 1)
    }

    if (number <= 1) false
    else isPrimeTailRec(number - 1)
  }

  println(greet("Diego", 25))
  println(factorial(0))
  println(fibo(10))
  println(isPrime(29))
}
