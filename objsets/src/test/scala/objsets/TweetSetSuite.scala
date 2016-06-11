package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      val sut = new Empty

      val result = sut.filter(tw => tw.user == "a")

      assert(size(result) === 0)
    }
  }

  test("filter: can filter on a non empty set") {
    val a = new Tweet("a", "a body", 20)
    val b = new Tweet("b", "b body", 20)
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)

    val set5 = new Empty().incl(a).incl(b).incl(c).incl(d)

    assert(size(set5.filter(tw => tw.user == "a")) === 1)
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("Union: empty union X is X") {
    val empty = new Empty()
    val nonEmpty = new Empty().incl(new Tweet("a", "a body", 20))

    val result = empty.union(nonEmpty)

    assert(result == nonEmpty)
  }

  test("Union: set4c and set4d") {
    val a = new Tweet("a", "a body", 20)
    val b = new Tweet("b", "b body", 20)
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)

    val set3 = new Empty().incl(a).incl(b)
    val set4c = new Empty().incl(a).incl(b).incl(c)
    val set4d = new Empty().incl(a).incl(b).incl(d)
    val set5 = new Empty().incl(a).incl(b).incl(c).incl(d)

    val result = set4c.union(set4d)

    assert(size(result) === 4)
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }
/*
  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
*/
}
