#!/usr/bin/env scala

object Main extends App {
  def retAnswer(n: Int, k: Int, ch: List[Int]): String =
    "TAK"

  // insert your code here
  var tests = Console.readInt
  for (i <- 1 to tests) {
    var params = Console.readLine().split(" ").map(_.toInt).toList

    var channels = Console.readLine().split(" ").map(_.toInt).toList
    val ans = retAnswer(params(0), params(1), channels)
    print(ans)
  }
}
Main.main(args)