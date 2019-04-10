package patmat

object Main extends App {
  def retAnswer(n: Int, k: Int, ch: List[Int]): String = {
    var mid = 0
    var sequence = scala.collection.mutable.Map[Int, Int]()
    for (c <- ch) {
      if ((c <= n) & (c >= 1)) {
        if (c == n) {
          if (shallStop(sequence.size + 1, n, mid)) {
            return "TAK"
          }
        }
        else sequence(c) = c

        if (c != 1 && c != n) mid += 1
      }

    }
    "NIE"
  }

  def shallStop(size: Int, n: Int, mid: Int): Boolean =
    (size == n) && (mid == (n - 2)) || ((n == 1) && size == 1)

  def wrSub(numb: List[Int], iter: Int): String = {
    if (iter == 0) numb.reverse.map(_.toChar).toString()
    val head = numb.head
    if (head > 0) wrSub((head - 1) :: numb.tail, iter - 1)
    else wrSub(numb.tail, iter - 1)
  }

  // insert your code here
  //var tests = Console.readInt
  var params = Console.readLine().split(" ").toList
  val number = params(0).toList
  val iter = params(1).toInt
  val result = wrSub(number.reverse.map(_.toInt), iter)

  //  }
}
