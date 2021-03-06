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
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
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
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains intersection of 2 sets") {
    new TestSets {
      val s = union(s1, s2)
      val d = union(s2, s3)
      val inter = intersect(s, d)
      assert(!contains(inter, 1), "1 is only in s")
      assert(contains(inter, 2), "2 is in both sets")
      assert(!contains(inter, 3), "3 is only in d")
    }
  }

  test("diff contains difference of 2 sets") {
    new TestSets {
      val s = union(s1, s2)
      val d = union(s2, s3)
      val inter = diff(s, d)
      assert(contains(inter, 1), "1 is only in s - shouldn't be removed by diff")
      assert(!contains(inter, 2), "2 is in both sets - should be removed by diff")
      assert(contains(inter, 3), "3 is only in d - shouldn't be removed by diff")
    }
  }

  test("filter returns s filtered by function") {
    new TestSets {
      val s = union(s1, s2)
      val flt = filter(s, x => x > 1)
      assert(!contains(flt, 1), "1 shouldn be filtered")
      assert(contains(flt, 2), "2 shouldn't be filtered")
    }
  }

  test("forall checks if all values in s within bounds satisfies p ") {
    new TestSets {
      val s = (x: Int) => x < 200
      val s5 =(x:Int) => x==3 || x==1
      val b = (x: Int) => x < 200 && x>0
      val p = (x: Int)=> x > 0
      val p5 =(x:Int) => x==2 || x==1
      assert(!forall(s,p), "not all bounded are grater than zero")
      assert(forall(b,p), "all elements in b are bounded and in p")
      assert(!forall(s5,p5), "not all bounded elements of s2 are within p2")

    }
  }

  test("exists checks if exists value in s within bounds satisfies p ") {
    new TestSets {
      val s = (x: Int) => x < 200
      val s5 =(x:Int) => x==3 || x==1
      val p = (x: Int)=> x > 0
      val p5 =(x:Int) => x==2
      assert(exists(s,p), "there are bounded values in s which are in p")
      assert(!exists(s5,p5), "no bounded elements in s are in p")
    }
  }

  test("map applies p to all elements of s") {
    new TestSets {
      val s = (x: Int) => x==3 || x==2
      val p = (x: Int)=> x * x
      assert(contains(map(s,p),9), "the set after map should contain 9")
      assert(contains(map(s,p),4), "the set after map should contain 4")
      assert(!contains(map(s,p),3), "the set after map shouldn't contain 3")
      assert(!contains(map(s,p),2), "the set after map shouldn't contain 2")
    }
  }
}
