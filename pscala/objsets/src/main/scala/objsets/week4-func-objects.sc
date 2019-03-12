import objsets.NonEmpty
import scala.collection.immutable._

trait List[+T] {
  def head: T

  def tail: List[T]

  def isEmpty: Boolean

  def prepend[U>:T](elem:U):List[U] = new Cons(elem,this)

  def foreach(f: T => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }


}

object Nil extends List[Nothing] {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object List {
//  //  List(1,2)
//  def apply[T](x1: T, x2: T): Cons[T] = new Cons(x1, new Cons(x2, new Nil))
//  def apply() = new Nil
}

val t :List[String ] = List("ora","appa","test")
val tst= List(1,2)
val x: List[String] = Nil


//tst.head
//tst.tail.head


//val a:Array[NonEmpty] = new Array[1,NonEmpty,NonEmpty]
//val b: Array[IntSet] = a
//
//type A = IntSet => NonEmpty
//type B = NonEmpty => IntSet

