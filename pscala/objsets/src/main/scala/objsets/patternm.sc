import exprs.show

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1,e2) => e1.eval * e2.eval
  }
}

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(n: String) extends Expr

object exprs{
  def show(e:Expr):String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => "("+show(e1) +" + "+ show(e2)+")"
    case Prod(e1,e2) => show(e1) +" * " + show(e2)
    case Var(n) => n

  }
}

val test = Sum(Number(3),Number(4))
show(test)
show(Number(3))
show(Sum(Prod(Number(2),Var("x")),Var("y")))
show(Prod(Var("x"),Number(4)) )

