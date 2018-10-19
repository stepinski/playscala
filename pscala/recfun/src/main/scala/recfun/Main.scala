package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def sumrow(xs: Vector[Int]): Vector[Int] = {
      if (xs.length == 1) xs
      else {
        sumrow(xs.tail) :+ (xs.head + xs.tail.head)
      }
    }

    def rows(n: Int): Vector[Int] = {
      if (n == 0) Vector(1)
      else if (n == 1) Vector(1, 1)
      else sumrow(rows(n - 1)) :+ 1
    }

    rows(r)(c)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def bal(xs: List[Char], isOpened: Int): Boolean = {
      if (xs.isEmpty && isOpened == 0) true
      else {
        val head = xs.head.toInt
        val end = xs.last.toInt
        val opened = isOpened

        if (head == 40) bal(xs.tail, opened + 1)
        else if (head == 41 && opened > 0) bal(xs.tail, opened - 1)
        else if (head == 41 && opened == 0) false
        else bal(xs.tail, opened)
      }
    }

    bal(chars,0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
