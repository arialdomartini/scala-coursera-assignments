package objsets

import org.scalatest.FunSuite
import GoogleVsApple._

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

  test("mostRetweeted: return the tweet with the highest number of retweets") {
    val a = new Tweet("a", "a body", 20)
    val b = new Tweet("b", "b body", 30)
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)

    val set = new Empty().incl(a).incl(b).incl(c).incl(d)

    val result = set.mostRetweeted

    assert(result === b)
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("size returns the number of all the non empty TweetSet present") {
    val a = new Tweet("a", "a body", 20)
    val b = new Tweet("b", "b body", 30)
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    
    val set = new Empty().incl(b).incl(a).incl(c).incl(d)
    
    val result = set.size
    
    assert(result === 4)
  }

  test("size returns the number of all the non empty TweetSet present in a list") {
    val a = new Tweet("a", "a body", 20)
    val b = new Tweet("b", "b body", 30)
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    
    val set = new Cons(a, new Cons(b, new Cons(c, new Cons(d, Nil))))
    
    val result = set.size
    
    assert(result === 4)
  }

  test("descending: orders tweets by retweets") {
    val a = new Tweet("a", "5 body", 20)
    val b = new Tweet("b", "2 body", 30)
    val c = new Tweet("c", "7 body", 7)
    val d = new Tweet("d", "1 body", 9)
    
    val set = new Empty().incl(a).incl(b).incl(c).incl(d)
    
    val result = set.descendingByRetweet
    
    assert(result.head.user == "b")

    assert(result.size === 4)
  }

  test("can read google and apple tweets") {
    val result = TweetReader.allTweets

    assert(size(result) === 695)
  }

  test("string contains one of the items listed in a list") {
    val list = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")

    assert(contains("this is android", list) === true)
    assert(contains("nexus and Nexus", list) === true)
    assert(contains("this is xxx", list) === false)
  }

  test("should filter Google tweets") {
    assert(GoogleVsApple.googleTweets.size === 31)
  }

  test("should filter Apple tweets") {
    assert(GoogleVsApple.appleTweets.size === 105)
  }
  test("should filter Apple and Googletweets") {
    assert(GoogleVsApple.appleAndGoogleTweets.size === 129)
  }

  test("trending should contain google and apple tweets") {
    assert(trending.size === 129)
  }
}
