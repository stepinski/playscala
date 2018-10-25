object ex2 {
  def sumr(f: Int => Int, a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, zero = 0)(a, b)

  //  {
  //    def loop(a: Int, acc: Int): Int = {
  //      if (a > b) acc
  //      else loop(a + 1, f(a) + acc)
  //    }
  //
  //    loop(a, 0)
  //  }

  sumr(x => x, 1, 3)

  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  //    if(a > b ) 1
  //    else f(a) * product(f)(a+1,b)

  product(x => x * x)(2, 3)

  def factorial(a: Int) = product(x => x)(1, a)

  factorial(3)

  import math.abs

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
     // println("guess = " + guess)
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x/2)(1)

  fixedPoint(x => 1 + x/3)(1)

  def sqrt(x: Double) = fixedPoint(averageDamp(y=>x/y))(1.0)

 // sqrt(2.0)
  sqrt(2)


  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2


}



