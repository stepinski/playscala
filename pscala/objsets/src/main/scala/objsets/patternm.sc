import exprs.show

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}

case class Number(n: Int) extends Expr
case class Sum(e1: Number, e2: Number) extends Expr

object exprs{
  def show(e:Expr):String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) =>   show(e1) +" + "+ show(e2)
  }
}

val test = Sum(Number(3),Number(4))
show(test)
show(Number(3))

