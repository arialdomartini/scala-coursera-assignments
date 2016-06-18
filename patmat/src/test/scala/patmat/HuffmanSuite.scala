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

/*

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

  test("singleton should detect wether a list contains only one or more code tree") {
    assert(singleton( List(Leaf('a', 1)) ) == true)

    assert(singleton( List(Leaf('a', 1), Leaf('b', 5)) ) == false)

    assert(singleton( List() ) == false)
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

    val expected =
      Fork(
        Leaf('x', 4), 
        Fork(
          Leaf('t', 2), 
          Leaf('e', 1), 
          List('t', 'e'), 3), 
        List('x', 't', 'e'), 7)


    assert(result == expected)
  }
  
  test("creates a complete tree starting from a string") {
    val input = "ettxxxx"

    val result = createCodeTree(string2Chars(input))

    val expected =
      Fork(
        Leaf('x',4),
        Fork(
          Leaf('t', 2),
          Leaf('e', 1),
          List('t', 'e'), 3
        ),
        List('x', 't', 'e'), 7
      )

    assert(result == expected)
  }

  test("my times") {
    val input = "cc adbea babcd aaba edd bc ab a cbaa"
    // 10 a
    // 8 space
    // 7 b
    // 5 c
    // 4 d
    // 2 e

    val expected = List(('a', 10), (' ', 8), ('b', 7), ('c', 5), ('d', 4), ('e', 2))

    assert(times(input.toList) === expected)
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

    val expected = 
      Fork(
        Fork(
          Fork(
            Fork(
              Fork(
                Leaf('e', 2),
                Leaf('d', 4),
                List('e', 'd') ,6),
              Leaf('c', 5),
              List('e', 'd', 'c'), 11),
            Leaf('b', 7),
            List('e', 'd', 'c', 'b'), 18),
          Leaf(' ', 8),
          List('e', 'd', 'c', 'b', ' ' ) ,26),
        Leaf('a', 10),
        List('e', 'd', 'c', 'b',' ' , 'a'),36)

    assert(result == expected)
  }

  test("creates a complete tree starting from a string, simple case") {
    val input = "aaaaxaaaxaaa"
    // 10 a
    // 2 x

    val result = createCodeTree(string2Chars(input))

    val expected = Fork(
      Leaf('a', 10),
      Leaf('x', 2),
      List('a', 'x'),12)

    assert(result == expected)
  }

  test("should decode the frenchCode") {
    assert(decode(frenchCode, secret) == string2Chars("huffmanestcool"))
  }

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
    val tree = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)

    val result = convert(tree)

    assert(result === List(('b', List(1)), ('a', List(0))))
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
      ('x', List(1, 1, 1, 1)),
      ('a', List(1, 1, 0, 0)),
      ('b', List(1, 0, 0, 0)),
      ('c', List(0, 1, 0, 0))
    )

    assert(result === expected)
  }

  test("my decode and quick encode is identity") {
    val baseForCodeTree = string2Chars("ture from 45 BC, making it over 2000 years old. Richard Mc")
    val text = string2Chars("ture from 45 BC, making it over 2000 years old. Richard Mc")
    val codeTree = createCodeTree(baseForCodeTree)

    assert(decode(codeTree, quickEncode(codeTree)(text) ) === text )
  }

  test("should encode") {
    
  }
  */
}
