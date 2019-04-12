import patmat.Huffman._
import sun.security.ssl.SSLContextImpl.TLS10Context


val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
def decodeChar(tree: CodeTree, root: CodeTree, bits: List[Bit], ret: String): String = {
  tree match {
    case Fork(left, right, chars, w) =>
      println(w)
      if (bits.head == 0) {
        decodeChar(left, root, bits.tail, ret)
      }
      else {
        decodeChar(right, root, bits.tail, ret)
      }
    case Leaf(ch, w) =>
      println(ch)
      var retVal = ret :+ ch
      if (bits.isEmpty) {
        retVal
      }
      else
        decodeChar(root, root, bits, retVal)
  }
}

/**
  * This function decodes the bit sequence `bits` using the code tree `tree` and returns
  * the resulting list of characters.
  */
def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  var tmp = decodeChar(tree, tree, bits, "")
  tmp toList
}

def convert(tree: CodeTree): CodeTable =
  tmpConvert(tree, List[Bit](), List[(Char, List[Bit])]())


def tmpConvert(tree: CodeTree, bits: List[Bit],
               tab: CodeTable): CodeTable = tree match {
  case Fork(left, right, chars, w) =>
    tmpConvert(left, bits :+ 0, tab):::tmpConvert(right, bits :+ 1, tab)
  case Leaf(ch, w) =>
    //    println(ch)
    //    println(ch+"|| "+tab)
    (ch, bits)::tab

}

def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  val ct = convert(tree)
  println(ct)
  var bits = List[Bit]()
  for (c <- text) {
    ct.find(x => x._1 == c) match {
      case Some(x) => bits = bits ::: x._2;
      case _ => List.empty[Bit]
    }
  }
  bits
}




convert(t2)


encode(t1)("ab".toList)

val tst = quickEncode(t1)("ab".toList)

tst

decode(t1, tst)

//decode(t1, List(0, 1))

decode(t1, List(0, 1))
frenchCode
decodeChar(t1, t1, List(0, 1), "")

encode(frenchCode)("dfnaoifdjasfdiooi".toList)


var t = convert(t1)
t(0)


//decodeChar(t1,List(0, 1),"")













