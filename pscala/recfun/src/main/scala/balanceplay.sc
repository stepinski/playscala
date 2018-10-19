val test1 = "(if (zero? x) max (/ 1 x))".toList
val testb = ":-)".toList
import scala.compat.Platform.EOL
test1.head.toInt
test1.reverse.head.toInt

def balance(xs: List[Char], isOpened: Int): Boolean = {
  if (xs.isEmpty && isOpened == 0) true
  else {
    val head = xs.head.toInt
    val end = xs.last.toInt
    val opened = isOpened
    print(" iter: " + xs.head + " " + isOpened + " " )

    if (head == 40) balance(xs.tail,opened+1)
    else if (head == 41 && opened>0) balance(xs.tail, opened-1)
    else if(head==41 && opened==0) false
    else balance(xs.tail, opened)
  }
}
balance(testb,0)




//balance(test1,false)
