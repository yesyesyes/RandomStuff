package com.appspot.yesopodelkinascale

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.Queue

@RunWith(classOf[JUnitRunner])
class SortingTests extends FunSuite with ShouldMatchers {
  
  import Sorting.insertion._

  test("insertion sort test") {
    val arr = Array(5, 2, 6, 3, 1)
    insertion(arr) should equal(Array(1, 2, 3, 5, 6))
  }

  test("funInsertion sort test") {
    val ls = List(5, 2, 6, 3, 1)
    funInsertion(ls) should equal(Queue(1, 2, 3, 5, 6))
  }

  
  import Sorting.merge._
  
  test("merge test") {
    val arr = Array(7, 1, 4, 6, 2, 5, 10)
    merge(arr, 1, 3, 5) should equal(Array(7, 1, 2, 4, 5, 6, 10))
  }
  
  test("funMerge") {
    val l = List(1, 3, 5, 56)
    val r = List(2, 4, 7, 55)
    funMerge(l, r) should equal(List(1,2,3,4,5,7, 55, 56))
  }
  
  import Sorting.heap._

}