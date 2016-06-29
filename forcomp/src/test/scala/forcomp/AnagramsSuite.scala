package forcomp

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite with Matchers  {
  test("should convert a string to a list of chars") {
    val string = "abc"

    val result = stringToChars(string)

    assert(result === List('a', 'b', 'c'))
  }

  test("wordOccurrences: empty string") {
    val result = wordOccurrences("")

    assert(result === List())
  }

  test("wordOccurrences: string with a single char") {
    val result = wordOccurrences("a")

    assert(result === List(('a', 1)))
  }

  test("wordOccurrences: string with a repetition of a single char") {
    val result = wordOccurrences("aaaa")

    assert(result === List(('a', 4)))
  }

  test("wordOccurrences: string with multiple chars, sorted by the characters in an ascending order") {
    val result = wordOccurrences("bbaaaabbb")

    assert(result === List( ('a', 4), ('b', 5) ))
  }

  test("wordOccurrences: ignores case") {
    val result = wordOccurrences("aaAA")

    assert(result === List(('a', 4)))
  }


  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }
  
  test("sentenceOccurrences: no words") {
    val result = sentenceOccurrences( List() )

    assert(result === List())
  }

  test("sentenceOccurrences: single word is just like wordOccurrences of the provided word") {
    val result = sentenceOccurrences( List("word") )

    assert(result === List(('d', 1), ('o', 1), ('r', 1), ('w', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("dictionaryByOccurrences.get: empty") {
    assert(dictionaryByOccurrences(List(('x', 1000))) === List())
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("combs: []") {
    val result = combs(List())

    assert(result === List(Nil))
  }

  test("combs: [1, 2]") {
    val list = List(1, 2)
    val expected = List(
      List(),
      List(1),
      List(2),
      List(1, 2)
    )

    val result = combs(list)

    result should contain theSameElementsAs expected
  }

  test("suboccurrences") { 
    val occurrence = ('a', 10)
    val expected = List(('a', 1),  ('a', 2),  ('a', 3),  ('a', 4),  ('a', 5),  ('a', 6),  ('a', 7),  ('a', 8),  ('a', 9),  ('a', 10))

    val result = suboccurrences(occurrence)

    assert(result === expected )
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: baaa - ba == aa") {
    val baaa = List(('b', 1), ('a', 3))
    val ba = List(('b', 1), ('a', 1))
    val aa = List(('a', 2))
    assert(subtract(baaa, ba) === aa)
  }


  test("sentence anagrams: []") {
    val sentence = List()
    
    val result = sentenceAnagrams(sentence)

    assert(result === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }
}
