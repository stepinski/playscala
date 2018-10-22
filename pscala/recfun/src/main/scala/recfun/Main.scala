package recfun

object Main {
  def main(args: Array[String]) {
    println("wyniki1")
    // countChange2(5, List(2, 3, 1), 0)
    println("wyniki2")
    countChange(4, List(1, 2))

    println("Pascal's Triangle")

    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("wyniki1")
    countChange2(5, List(2, 3, 1), 0)
    println("wyniki2")
    countChange2(4, List(1, 2), 0)
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

    bal(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def innerCount(money: Int, pathSum: Int, coins: List[Int], cntr: Int): Int = {
      if (coins.nonEmpty) {
        val head = coins.head
        // print("|" + money + " " + pathSum + " " + coins.length + "|")
        //the problem can be decomposed to moving on tree - each nodes have next nodes with weights= coins values
        // the problem would be defined as counting path lenghts ( sum of coin values) - at each level we can or build next set of paths or skip
        // to path with value from another coin
        if (pathSum < money) {
          innerCount(money, pathSum + head, coins, cntr) + innerCount(money, pathSum, coins.tail, cntr)
        } else if (pathSum == money) {
          //  print(" ok ")
          cntr + 1
        }
        else cntr
      } else cntr
    }

    innerCount(money, 0, coins, 0)
  }

  def countChange2(money: Int, coins: List[Int], cntr: Int): Int = {
    def innerCount(money: Int, total: Int, coins: List[Int], cntr: Int): Int = {
      if (coins.nonEmpty) {
        val head = coins.head
        if (money >= head) {
          innerCount(money - head, total, coins, cntr) + innerCount(money, total, coins.tail, cntr)
        } else if (money == 0)
          innerCount(total, total, coins.tail, cntr + 1)
        else innerCount(money, total, coins.tail, cntr) // innerCount(total, total, coins.tail, cntr)
      } else cntr
    }

    innerCount(money, money, coins, 0)
  }
}
