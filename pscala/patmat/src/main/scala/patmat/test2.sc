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
def wrSub(numb: List[Int], iter: Int): String = {
  if (iter == 0) numb.reverse.map(x=>(x+48).toChar).mkString
  else {
    val head = numb.head
    print("|"+head+"|")
    if (head > 0) wrSub((head - 1) :: numb.tail, iter - 1)
    else wrSub(numb.tail, iter - 1)
  }
}
val str  = "512".toList
val it = "4".toInt

wrSub(str.reverse.map(_.asDigit), it)

def shallStop(size: Int, n: Int, mid: Int): Boolean =
  (size == n) && (mid == (n - 2)) || ((n == 1) && size == 1)

retAnswer(3, 6, List(5, 1, 1, 3, 2, 3))
retAnswer(3, 6, List(5, 1, 1, 3, 2, 2))
retAnswer(5, 6, List(4, 1, 1, 2, 3, 5, 4))
