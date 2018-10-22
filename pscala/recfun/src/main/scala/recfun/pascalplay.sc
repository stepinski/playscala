object session {
  def sumrow(xs: Vector[Int]): Vector[Int] = {
    if (xs.length == 1) xs
    else {
      sumrow(xs.tail) :+ (xs.head + xs.tail.head)
    }
  }
  sumrow(Vector(1, 3, 3, 1)) :+ 1
  sumrow(Vector(1, 2, 1)) :+ 1

  def rows(n: Int): Vector[Int] = {
    if (n == 0) Vector(1)
    else if (n == 1) Vector(1, 1)
    else sumrow(rows(n - 1)) :+ 1
  }

  rows(2)
  rows(3)
  rows(5)
  rows(5)(3)

  def pascal(c: Int, r: Int): Int = {
    rows(r)(c)
  }

  pascal(3,5)
 // pascal(100,5000)

2+3





}