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
    val leaf = Leaf('a', 2)
    assert(weight(leaf) === 2)
  }

  test("weight of a larger tree") {
	val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)

    assert(weight(t1) === 5)
  }

  test("should calculate the number of chars in a tree") {
    val tree = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a','b'), 5), Leaf('c', 4), List('a','b','c'), 9)

    val result = chars(tree)

    assert(result === List('a', 'b', 'c'))
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }
  
  test("REMOVE ME....................createCodeTree") {
    val a = Leaf('a', 3)
    val b = Leaf('b', 2)
    val c = Leaf('c', 1)
    val cb = Fork(c, b, List('c', 'b'), 3)
    val cb_a = Fork(cb, a, List('c', 'b', 'a'), 6)
    assert(createCodeTree(List('a', 'b', 'a', 'c', 'b', 'a')) === cb_a)
  }

  test("creates a complete tree starting from a string") {
    val input = "ettxxxx"

    val result = createCodeTree(string2Chars(input))

    val expected =
      Fork(
        Fork(
          Leaf('e', 1),
          Leaf('t', 2),
          List('e', 't'), 3
        ),
        Leaf('x',4),
        List('e', 't', 'x'), 7
      )
    assert(result == expected)
  }


  test("should count the occourrences of a char in a list") {
    val list = List('a', 'b', 'a')

    val result = times(list)

    assert(result === List(('b', 1), ('a', 2)))
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

  test("singleton should detect wether a list contains only one or more code tree") {
    assert(singleton( List(Leaf('a', 1)) ) == true)

    assert(singleton( List(Leaf('a', 1), Leaf('b', 5)) ) == false)

    assert(singleton( List() ) == false)
  }

  test("makeOrderedLeafList for some frequency table") {
    val list = List(('t', 2), ('e', 1), ('x', 3))

    val result = makeOrderedLeafList(list)

    assert(result === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

    val result = combine(leaflist)

    val expected = List(
      Fork(
        Leaf('e',1), 
        Leaf('t',2), 
        List('e', 't'), 3), 
      Leaf('x',4))

    assert(result == expected)
  }

  test("until") {

    val list = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

    val result = until(singleton, combine)(list)

    val expected = Fork(
      Fork(
        Leaf('e', 1),
        Leaf('t' ,2),
        List('e', 't'), 3),
      Leaf('x', 4),
      List('e', 't', 'x'), 7
    )

    assert(result.head == expected)
  }


  test("creates a complete tree starting from a string, another more complex case") {
    val input = "cc adbea babcd aaba edd bc ab a cbaa"
    // 10 a
    // 8 space
    // 7 b
    // 5 c
    // 4 d
    // 2 e

    val result = createCodeTree(string2Chars(input))
    val a = Leaf('a', 10)
    val space = Leaf(' ', 8)
    val b = Leaf('b', 7)
    val c = Leaf('c', 5)
    val d = Leaf('d', 4)
    val e = Leaf('e', 2)

    val expected = 
      Fork(
        Fork(b, space, List('b', ' ' ), 15),
        Fork(a, Fork( c, Fork( e, d,
              List('e', 'd'), 6),
            List('c', 'e', 'd'), 11),
          List('a', 'c', 'e', 'd'), 21),
        List('b', ' ' , 'a', 'c', 'e', 'd'), 36)

    assert(result == expected)
  }
  
  test("createCodeTree") {
    val a = Leaf('a', 3)
    val b = Leaf('b', 2)
    val c = Leaf('c', 1)
    val cb = Fork(c, b, List('c', 'b'), 3)
    val cba = Fork(cb, a, List('c', 'b', 'a'), 6)
    assert(createCodeTree(List('a', 'b', 'a', 'c', 'b', 'a')) === cba)
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



  
  test("my times") {
    val input = "cc adbea babcd aaba edd bc ab a cbaa"
    // 10 a
    // 8 space
    // 7 b
    // 5 c
    // 4 d
    // 2 e

    val expected = List(('e', 2), ('d', 4), ('c', 5), ('b', 7), (' ', 8), ('a', 10))

    assert(times(input.toList) === expected)
  }

  test("creates a complete tree starting from a string, simple case") {
    val input = "aaaaxaaaxaaa"
    // 10 a
    // 2 x

    val result = createCodeTree(string2Chars(input))

    val expected = Fork(
      Leaf('x', 2),
      Leaf('a', 10),
      List('x', 'a'),12)

    assert(result == expected)
  }






  test("should decode the frenchCode") {
    assert(decode(frenchCode, secret) == string2Chars("huffmanestcool"))
  }
/////////////////////////// xxxxxxx
  test("decodedSecret should return the frenchCode") {
    assert(decodedSecret == string2Chars("huffmanestcool"))
  }

  test("decode and encode a very short text should be identity") {
    val tree = Fork(Leaf('a', 2), Leaf('b', 3), List('a','b'), 5)

    val encoded = encode(tree)("ab".toList)

    val result = decode(tree, encoded)

    assert(result === "ab".toList)
  }

  test("codeBits returns the bits representation of a char based on a codeTable") {
    val codeTable = List(
      ('a', List(1, 1, 0, 0)),
      ('b', List(1, 0, 0, 0)),
      ('c', List(0, 1, 0, 0))
    )

    assert(codeBits(codeTable)('a') == List(1, 1, 0, 0))
    assert(codeBits(codeTable)('b') == List(1, 0, 0, 0))
    assert(codeBits(codeTable)('c') == List(0, 1, 0, 0))
  }


  test("convert builds a code table") {
    val tree = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)

    val result = convert(tree)

    assert(result === List(('b', List(1)), ('a', List(0))))
  }


  test("convert test") {
    val tree = 
      Fork(
        Fork(
          Leaf('a', 2), 
          Leaf('b', 3),
          List('a', 'b'), 5),
        Leaf('d', 10),
        List('a', 'b', 'd'), 15)

    val result = convert(tree)

    assert(result === List(('d', List(1)), ('b', List(1, 0)), ('a', List(0, 0))))

  }

  test("mergeCodeTables merges two different code tables") {
    val codeTable1 = List(
      ('a', List(1, 1, 0, 0)),
      ('b', List(1, 0, 0, 0)),
      ('c', List(0, 1, 0, 0))
    )
    val codeTable2 = List(
      ('b', List(1, 0, 0, 0)),
      ('a', List(1, 1, 0, 0)),
      ('x', List(1, 1, 1, 1))
    )
    
    val result = mergeCodeTables(codeTable1, codeTable2)

    val expected = List(
      ('b', List(1, 0, 0, 0)),
      ('a', List(1, 1, 0, 0)),
      ('c', List(0, 1, 0, 0)),
      ('x', List(1, 1, 1, 1))
    )

    assert(result === expected)
  }

/*
  test("my decode and quick encode is identity") {
    val text = string2Chars("ture from 45 BC, making it over 2000 years old. Richard Mc")
    val codeTree = createCodeTree(text)

    assert(decode(codeTree, quickEncode(codeTree)(text) ) === text )
  }
*/

  test("my decode and encode is identity") {
    val text = string2Chars("I should get this text back after encode and decode")
    val codeTree = createCodeTree(text)
    println(codeTree.toString)

    assert(decode(codeTree, encode(codeTree)(text) ) === text )
  }
}
