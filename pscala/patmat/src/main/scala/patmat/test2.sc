//import scala.util.control.Breaks._

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

retAnswer(3, 6, List(5, 1, 1, 3, 2, 3))
retAnswer(3, 6, List(5, 1, 1, 3, 2, 2))
retAnswer(5, 6, List(4, 1, 1, 2, 3, 5, 4))
