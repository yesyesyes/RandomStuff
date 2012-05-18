package com.appspot.yesopodelkinascale

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.Queue

@RunWith(classOf[JUnitRunner])
class SortingTests extends FunSuite with ShouldMatchers {
  import Sorting._

  test("insertion sort test") {
    val arr = Array(5, 2, 6, 3, 1)
    insertion(arr) should equal(Array(1, 2, 3, 5, 6))
  }

  test("funInsertion sort test") {
    val ls = List(5, 2, 6, 3, 1)
    funInsertion(ls) should equal(Queue(1, 2, 3, 5, 6))
  }

}