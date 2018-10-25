object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x.numer
  x.denom
  //x.sub(y).sub(z)
  x + y
  x - y

  x < y
  x.max(y).denom

  // a + b ^? c ?^ d less a ==> b) | c

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  gcd(6,9)

  class Rational(x: Int, y: Int) {
    require(y > 0, "denominator must be positive")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    //private val g = gcd(x, y)

    def numer = x

    def denom = y

    def < (that: Rational) = this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) = if (this < that) that else this

    def + (that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    def unary_- : Rational = new Rational(-numer, denom)

    def  - (that: Rational) = this + -that

    override def toString = {
      val g= gcd(numer, denom)
      numer/g + "/" + denom/g
    }
  }

}