package lectures.part1basics

object CBNvsCBV extends App {
  def calledByValue(x: Long): Unit = { // Eager evaluation (the parameter expression is evaluated upon function call)
    println("by value: " + x)
    println("by value: " + x)
  }

  def calledByName(x: => Long): Unit = { // Lazy evaluation (the parameter expression is evaluated where it is used at function - not pointer i guess)
    println("by name: " + x)
    println("by name: " + x)
  }

  calledByValue(System.nanoTime())
  calledByName(System.nanoTime())
}
