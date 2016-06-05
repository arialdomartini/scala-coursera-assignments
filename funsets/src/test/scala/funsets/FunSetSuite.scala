package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
    def negativeNumbers(x: Int) = if(x<0) true else false
    assert(contains(x => negativeNumbers(x), 100) === false)
    assert(contains(x => negativeNumbers(x), 0) === false)
    assert(contains(x => negativeNumbers(x), -1) === true)
    assert(contains(x => negativeNumbers(x), -100) === true)
  }

  test("not function inverts a predicate") {
    def predicate = (e: Int) => e % 2 == 0
    assert(predicate(1) === false)
    assert(predicate(2) === true)

    def actual = not(predicate)

    assert(actual(1) === true)
    assert(actual(2) === false)
  }
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1) === true, "Singleton")
      assert(contains(s2, 2) === true, "Singleton")
      assert(contains(s3, 3) === true, "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)

      assert(contains(s, 1) === true, "Union 1")
      assert(contains(s, 2) === true, "Union 2")
      assert(contains(s, 3) === false, "Union 3")
    }
  }

  test("intersect contains only elements that both sets contains") {
    new TestSets {
      val s = intersect(s1, s2)

      assert(contains(s, 1) === false, "intersect(s1, s2) does not contain s1")
      assert(contains(s, 2) === false, "intersect(s1, s2) does not contain s2")
    }

    new TestSets {
      val s = intersect(s1, s1)

      assert(contains(s, 1) === true, "intersect(s1, s1) contains s1")
      assert(contains(s, 2) === false, "intersect(s1, s2) does not contain s2")
    }
  }

  test("diff(s, t) returns a set with elements that are in s and not are present in t") {
    new TestSets {
      val s = union(s1, s2)
      val t = union(s1, s3)

      val actual = diff(s, t)

      assert(contains(actual, 1) === false)
      assert(contains(actual, 2) === true)
      assert(contains(actual, 3) === false)
    }
  }

  test("filter can define a new set based on another set, whose elements are selected based on a function") {
    new TestSets {
      val s = union(union(s1, s2), s3)

      val actual = filter(s, e => e == 1)

      assert(contains(actual, 1) === true)
      assert(contains(actual, 2) === false)
      assert(contains(actual, 3) === false)
    }

    new TestSets {
      val s = union(union(s1, s2), s3)

      val actual = filter(s, e => e > 1)

      assert(contains(actual, 1) === false)
      assert(contains(actual, 2) === true)
      assert(contains(actual, 3) === true)
    }
  }

  test("forall tests whether a given predicate is true for all elements of the set") {

    new TestSets {
      val s = union(union(s1, s2), s3)

      assert(forall(s, e => e == 2 || e == 3) === false)
      assert(forall(s, e => e == 1 || e == 2 || e == 3) === true)
      assert(forall(s, e => e > 0) === true)
      assert(forall(s, e => e < 1000) === true)
      assert(forall(s, e => e > 0 || e <= 3) === true)
    }
  }

  test("exists tests whether a set contains at least one element for which the given predicate is true") {

    new TestSets {
      val s = union(union(s1, s2), s3)

      assert(exists(s, e => e == 2 || e == 3) === true)
      assert(forall(s, e => e == 1 || e == 2 || e == 3) === true)
      assert(forall(s, e => e > 0) === true)
      assert(forall(s, e => e > 10) === false)
      assert(forall(s, e => e > 0 || e <= 3) === true)
    }
  }

  test("map which transforms a given set into another one by applying to each of its elements the given function") {

    new TestSets {
      val s = union(union(s1, s2), s3)

      val actual = map(s, e => 10 * e)

      assert(contains(actual, 1) === false)
      assert(contains(actual, 2) === false)
      assert(contains(actual, 3) === false)
      assert(contains(actual, 10) === true)
      assert(contains(actual, 20) === true)
      assert(contains(actual, 30) === true)
    }
  }

}
