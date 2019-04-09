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

  // insert your code here
  //var tests = Console.readInt
  var params = Console.readLine().split(" ").toList
  val numbers = params(0).toList
  val iters = params(1).toInt


//  }
}
