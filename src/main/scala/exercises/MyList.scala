package exercises

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
}

object ListTest extends App {
  private val listOfIntegers = new Cons(1, new Cons(2, new Cons(3, Empty)))
  private val listOfStrings = new Cons("Hello", new Cons("Scala", new Cons("Learner", Empty)))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  println(listOfIntegers.filter(new Function1[Int, Boolean] {
    override def apply(element: Int): Boolean = element % 2 == 0
  }).toString)

  println(listOfIntegers.map(new Function1[Int, Int] {
    override def apply(element: Int): Int = element * 2
  }).toString)

  println(listOfIntegers.flatMap(new Function1[Int, MyList[Int]] {
    override def apply(element: Int): MyList[Int] = new Cons(element, new Cons(element + 1, Empty))
  }).toString)
}
