import patmat.Huffman.{CodeTree, combine, _}
import sun.security.ssl.SSLContextImpl.TLS10Context


val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
val t3 = Fork(Leaf('a', 8), Fork(Fork(Leaf('b', 3), Fork(Leaf('c', 1), Leaf('d', 1), List('c', 'd'), 2), List('b', 'c', 'd'), 5), Fork(Fork(Leaf('e', 1), Leaf('f', 1), List('e', 'f'), 2), Fork(Leaf('g', 1), Leaf('h', 1), List('g', 'h'), 2), List('e', 'f', 'g', 'h'), 4), List('b', 'c', 'd', 'e', 'f', 'g', 'h'), 9), List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 17)

def convert(tree: CodeTree): CodeTable =
  tmpConvert(tree, List[Bit](), List[(Char, List[Bit])]())


def tmpConvert(tree: CodeTree, bits: List[Bit],
               tab: CodeTable): CodeTable = tree match {
  case Fork(left, right, chars, w) =>
    tmpConvert(left, bits :+ 0, tab) ::: tmpConvert(right, bits :+ 1, tab)
  case Leaf(ch, w) =>
    //    println(ch)
    //    println(ch+"|| "+tab)
    (ch, bits) :: tab
}
def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  val ct = convert(tree)
  println(ct)
  var bits = List[Bit]()
  for (c <- text) {
    ct.find(x => x._1 == c) match {
      case Some(x) => bits = bits ::: x._2;
      case _ => bits = bits ::: List.empty[Bit]
    }
  }
  bits
}

def times(chars: List[Char]): List[(Char, Int)] =
  chars map (x => (x, chars.count(_ == x))) distinct

/**
  * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
  *
  * The returned list should be ordered by ascending weights (i.e. the
  * head of the list should have the smallest weight), where the weight
  * of a leaf is the frequency of the character.
  */
def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  freqs sortWith (_._2 < _._2) map (x => Leaf(x._1, x._2))

/**
  * Checks whether the list `trees` contains only one single code tree.
  */
def singleton(trees: List[CodeTree]): Boolean =
  (trees size) == 1

/**
  * The parameter `trees` of this function is a list of code trees ordered
  * by ascending weights.
  *
  * This function takes the first two elements of the list `trees` and combines
  * them into a single `Fork` node. This node is then added back into the
  * remaining elements of `trees` at a position such that the ordering by weights
  * is preserved.
  *
  * If `trees` is a list of less than two elements, that list should be returned
  * unchanged.
  */
def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
  case l if l.size <= 1 => l
  case l => ((l tail) tail) :+ makeCodeTree(l head, (l tail) head) sortWith (weight(_) < weight(_))
}

/**
  * This function will be called in the following way:
  *
  * until(singleton, combine)(trees)
  *
  * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
  * the two functions defined above.
  *
  * In such an invocation, `until` should call the two functions until the list of
  * code trees contains only one single tree, and then return that singleton list.
  *
  * Hint: before writing the implementation,
  *  - start by defining the parameter types such that the above example invocation
  * is valid. The parameter types of `until` should match the argument types of
  * the example invocation. Also define the return type of the `until` function.
  *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
  */
//  def until(xxx: ???, yyy: ???)(zzz: ???): ??? = ???
def until(isS: List[CodeTree] => Boolean, comb: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
  if (isS(trees)) {
//    println("test")
    trees
  }
  else {
//    println("test")
    List.empty[CodeTree]
    until(isS, comb)(comb(trees))
  }

/**
  * This function creates a code tree which is optimal to encode the text `chars`.
  *
  * The parameter `chars` is an arbitrary text. This function extracts the character
  * frequencies from that text and creates a code tree based on them.
  */
def createCodeTree(chr: List[Char]): CodeTree = {
  until(singleton, combine)(makeOrderedLeafList(times(chr))) head
}

quickEncode(t3)("abcdefghijklmnopqzzzzzzdfsafjadsjfoisdjfoajpfajdiosajfiosdajfpoasd".toList)
//val ct = convert(t3)
//ct
//quickEncode2(t3)("ab".toList)

//quickEncode(t3)("bac".toList)


//decode(t3, List(1, 0, 0, 0, 1, 0, 1, 0))

//quickEncode(t2)("ab".toList)

println("43433443")
//makeOrderedLeafList(times("bac".toList))
//def singleton2(trees: List[CodeTree]): Boolean =
//  (trees size) == 1


//val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
//combine(leaflist)

createCodeTree("bac".toList)
//val tst = singleton(List[CodeTree](t1))


//convert(t2)
//
//
//encode(t1)("ab".toList)
//
//val tst = quickEncode(t1)("ab".toList)
//
//tst
//
//decode(t1, tst)
//
////decode(t1, List(0, 1))
//
//decode(t1, List(0, 1))
//frenchCode
//decodeChar(t1, t1, List(0, 1), "")
//
//encode(frenchCode)("dfnaoifdjasfdiooi".toList)
//
//
//var t = convert(t1)
//t(0)
//
//
////decodeChar(t1,List(0, 1),"")













