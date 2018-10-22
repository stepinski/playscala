
def countChange(money: Int, coins: List[Int], cntr: Int): Int = {
  if (coins.nonEmpty) {
    val head = coins.head
    val cnt = cntr
    print(""+head + " " + cnt.toString + "|")
    if (money % head == 0) {
      countChange(money, coins.tail, cntr + 1) + countChange(money - head, coins.tail, cntr)
    }
        else if (money % head > 0) {
      countChange(money % head, coins.tail, cntr) + countChange(money, coins.tail, cntr)
    }
    else cntr
  } else cntr
}

countChange(5, List(2, 3, 1), 0)
countChange(4, List(1,2), 0)