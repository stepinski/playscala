import patmat.Huffman._
import sun.security.ssl.SSLContextImpl.TLS10Context


val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)

def decodeChar(tree: CodeTree, root: CodeTree, bits: List[Bit], ret: String): String = {

  tree match {
    case Fork(left, right, chars, w) =>
      if (bits.head == 0) {
        print("go left|")
        decodeChar(left, root, bits.tail, ret)
      }
      else {
        print(" go right |")
        decodeChar(right, root, bits.tail, ret)
      }
    case Leaf(ch, w) =>
      print(" mamleaf |")
      var retVal = ret :+ ch
      if (bits.isEmpty) {
        retVal
      }
      else
        decodeChar(root, root, bits, retVal)
  }

}

encode(t1)("ab".toList)
decode(t1, encode(t1)("ab".toList))

decode(t1, List(0, 1))

decode(t1, List(0, 1))
frenchCode
decodeChar(t1, t1, List(0, 1), "")

encode(frenchCode)("dfnaoifdjasfdiooi".toList)


var t = convert(t1)
t(0)


//decodeChar(t1,List(0, 1),"")













