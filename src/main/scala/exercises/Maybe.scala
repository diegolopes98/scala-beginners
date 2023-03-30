package exercises

abstract class Maybe[+A] {
  def isEmpty: Boolean
  def map[B](transformer: A => B): Maybe[B]
  def flatMap[B](transformer: A => Maybe[B]): Maybe[B]
  def filter(predicate: A => Boolean ): Maybe[A]
}
case object MaybeNot extends Maybe[Nothing] {
  def isEmpty: Boolean = true
  def map[B](transformer: Nothing => B): Maybe[B] = MaybeNot
  def flatMap[B](transformer: Nothing => Maybe[B]): Maybe[B] = MaybeNot
  def filter(predicate: Nothing => Boolean): Maybe[Nothing] = MaybeNot
}

case class Just[+A](value: A) extends Maybe[A] {
  def isEmpty: Boolean = false
  def filter(predicate: A => Boolean): Maybe[A] =
    if (predicate(value)) this
    else MaybeNot
  def map[B](transformer: A => B): Maybe[B] =
    Just(transformer(value))
  def flatMap[B](transformer: A => Maybe[B]): Maybe[B] =
    transformer(value)
}

object MaybeTest extends App {
  val just = Just(42)
  println(just)
  println(just.map[String](e => s"the answer to all questions is $e"))
  println(just.flatMap(e => Just(e * 42)))
  println(just.filter(_ % 2 != 0))
  println(just.filter(_ % 2 == 0))
}
