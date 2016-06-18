package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
	val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
	val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a single leaf") {
    val leaf = Leaf('a',2)
    assert(weight(leaf) === 2)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("should calculate the number of chars in a tree") {
    val tree = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('c',4), List('a','b','c'), 9)

    val result = chars(tree)

    assert(result === List('a', 'b', 'c'))
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("should update the list of char counts starting from an empty list") {
    val list = List()

    val result = countChar('a', list)

    assert(result == List(('a', 1)))
  }

  test("should update the list of char counts incrementing the count of the corresponding char") {
    val list = List(('a', 5))

    val result = countChar('a', list)

    assert(result == List(('a', 6)))
  }

  test("should update the list of char counts incrementing the count of the corresponding char and preserving all the other chars") {
    val list = List(('a', 5), ('b', 10))

    val result = countChar('b', list)

    assert(result == List(('a', 5), ('b', 11)))
  }

  test("should count the occourrences of a char in a list") {
    val list = List('a', 'b', 'a')

    val result = times(list)

    assert(result === List(('a', 2), ('b', 1)))
  }

  test("makeLeafList for a frequency table with one single element") {
    val list = List(('t', 11))

    val result = makeLeafList(list)

    assert(result === List(Leaf('t', 11)))
  }

  test("makeLeafList for some frequency table") {
    val list = List(('t', 2), ('e', 1), ('x', 3))

    val result = makeLeafList(list)

    assert(result === List(Leaf('t', 2), Leaf('e', 1), Leaf('x', 3)))
  }

  test("makeOrderedLeafList for some frequency table") {
    val list = List(('t', 2), ('e', 1), ('x', 3))

    val result = makeOrderedLeafList(list)

    assert(result === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("should insert a leaf in the right position in an ordered list") {
    val list = List(Leaf('c', 1), Leaf('a', 3), Leaf('d', 5))

    val result = insert(Leaf('b', 4), list)

    assert(result === List(Leaf('c', 1), Leaf('a', 3), Leaf('b', 4), Leaf('d', 5)))
  }

  test("should order a list of Leafs") {
    val list = List(Leaf('d', 5), Leaf('f', 15), Leaf('c', 1), Leaf('a', 3))

    val result = sort(list)

    assert(result == List(Leaf('c', 1), Leaf('a', 3), Leaf('d', 5), Leaf('f', 15)))
  }
  /*

test("combine of some leaf list") {
val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
  assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
}


  test("decode and encode a very short text should be identity") {
  new TestTrees {
  assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
  }
  }
  */
}
