object WrongSubstract extends App {
  def wrSub(numb: List[Int], iter: Int): String = {
    if (iter == 0) numb.reverse.map(x=>(x+48).toChar).mkString
    else {
      val head = numb.head
      if (head > 0) wrSub((head - 1) :: numb.tail, iter - 1)
      else wrSub(numb.tail, iter - 1)
    }
  }
  // insert your code here
  //var tests = Console.readInt
  var params = Console.readLine().split(" ").toList
  val number = params(0).toList
  val iter = params(1).toInt
  val result = wrSub(number.reverse.map(_.asDigit), iter)
  println(result)
}
