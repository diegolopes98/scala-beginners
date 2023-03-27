package exercises

import lectures.part2oop.Generics.MyList

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String
  override def toString: String = s"[$printElements]"
  def ++[B >: A](list: MyList[B]): MyList[B]
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean ): MyList[A]
  def forEach(func: A => Unit): Unit
  def sort(func: (A, A) => Int): MyList[A]
  def zipWith[B, C](list: MyList[B], func: (A, B) => C): MyList[C]
  def fold[B](acc: B)(func: (B, A) => B): B
}
case object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)
  def printElements: String = ""
  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
  def map[B](transformer: Nothing => B): MyList[B] = Empty
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty
  def forEach(func: Nothing => Unit): Unit = {}
  def sort(func: (Nothing, Nothing) => Int): MyList[Nothing] = Empty
  def zipWith[B, C](list: MyList[B], func: (Nothing, B) => C): MyList[C] =
    if (!list.isEmpty) throw new RuntimeException("Lists are not of same size")
    else Empty
  def fold[B](acc: B)(func: (B, Nothing) => B): B = acc
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyList[B] = new Cons(element, this)
  def printElements: String =
    if(t.isEmpty) s"$h"
    else s"$h, ${t.printElements}"
  def ++[B >: A](list: MyList[B]): MyList[B] = new Cons(h, t ++ list)
  def filter(predicate: A => Boolean): MyList[A] =
    if (predicate(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)
  def map[B](transformer: A => B): MyList[B] =
    new Cons(transformer(h), t.map(transformer))
  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)
  def forEach(func: A => Unit): Unit = {
    func(h)
    tail.forEach(func)
  }
  def sort(func: (A,A) => Int): MyList[A] = {
    def insert(x: A, sortedList: MyList[A]): MyList[A] =
      if (sortedList.isEmpty) new Cons(x, Empty)
      else if (func(x, sortedList.head) <= 0) new Cons(x, sortedList)
      else new Cons(sortedList.head, insert(x, sortedList.tail))

    val sortedTail = t.sort(func)
    insert(h, sortedTail)
  }
  def zipWith[B, C](list: MyList[B], func: (A, B) => C): MyList[C] =
    if (list.isEmpty) throw new RuntimeException("Lists are not of same size")
    else new Cons(func(h, list.head), t.zipWith(list.tail, func))

  def fold[B](acc: B)(func: (B, A) => B): B =
    t.fold(func(acc, h))(func)
}

object ListTest extends App {
  private val listOfIntegers = new Cons(1, new Cons(2, new Cons(3, Empty)))
  private val listOfStrings = new Cons("Hello", new Cons("Scala", new Cons("Learner", Empty)))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  println(listOfIntegers.filter(_ % 2 == 0).toString)

  println(listOfIntegers.map(_ * 2).toString)

  println(listOfIntegers.flatMap((e: Int) => Cons(e, Cons(e + 1, Empty))).toString)

  listOfStrings.forEach(println(_))

  println(listOfIntegers.zipWith[String, String](listOfStrings, _ + "-" + _))

  println(listOfIntegers.fold(0)((a: Int, b:Int) => a + b))
}
