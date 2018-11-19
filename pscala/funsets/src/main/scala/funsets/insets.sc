object insets{
  val t1= new NonEmpty(3,new Empty, new Empty)
  val t2= t1 incl 4
}

abstract class InSet {
  def contains(x:Int):Boolean
  def incl(x:Int): InSet
}

class NonEmpty(elem: Int, left:InSet, right:InSet) extends InSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true



class Empty extends InSet {
  def contains(x:Int):Boolean=false
  def incl(x:Int):InSet = new NonEmpty(x,new Empty,new Empty)
}



  def incl(x: Int): InSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
}
