package com.appspot.yesopodelkinascale

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.Queue
import scalaz._
import Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.GivenWhenThen
import org.scalatest.FunSpec

@RunWith(classOf[JUnitRunner])
class SortingTests extends FunSpec with GivenWhenThen {
  
  import Sorting.insertion._

  describe("insertion") {
    val arr = Array(5, 2, 6, 3, 1)
    it("should sorte array") {
      assert(insertion(arr) === arr.sorted)
    }
  }

  describe("funInsertion") {
    val ls = List(5, 2, 6, 3, 1)
    it("should sorte list") {
      assert(funInsertion(ls) === ls.sorted)
    }
  }

  import Sorting.merge._
  
  describe("merge x y z should sort array from x to z") {
    val arr = Array(7, 1, 4, 6, 2, 5, 10)
    it("should sorte x-z part of array") {
      assert(merge(arr, 1, 3, 5) === Array(7, 1, 2, 4, 5, 6, 10))
    }
  }
  
  describe("funMerge of two list l and r") {
    val l = List(1, 3, 5, 56)
    val r = List(2, 4, 7, 55)
    it("should produce new sorted list with all l and r elements") {
      assert(funMerge(l, r) === (l ++ r).sorted)
    }
  }
  
  import Sorting.heap._
  
  describe("maxHeapify on index x") {
    val arr = Array(16, 4, 10, 14, 7, 9, 3, 2, 8, 1)
    maxHeapify(arr, 1, arr.size)
    it("should reorder subtrees so that  properties are kept") {
      assert(arr === Array(16, 14, 10, 8, 7, 9, 3, 2, 4, 1))
    }
  }
  
  describe("buildMaxHeap should create max-heap from array") {
    val arr = Array(16, 4, 10, 14, 7, 9, 3, 2, 8, 1)
    buildMaxHeap(arr)
    it("should return max head") {
      assert(arr === Array(16, 14, 10, 8, 7, 9, 3, 2, 4, 1))
    }
  }

  describe("heap should sorte given array") {
    val arr = Array(16, 4, 10, 14, 7, 9, 3, 2, 8, 1)
    heap(arr)
    it("array after heapsort should be sorted") {
      assert(arr === arr.sorted)
    }
  }

  describe("funHeap") {
    val lst = List(16, 4, 10, 14, 7, 9, 3, 2, 8, 1)
    it("should sort the list") {
      assert(heapSort(lst) == lst.sorted)
    }
  }
  
  /* Priority queue tests */
  describe("queue max should return max element of the heap but don't retrieve it")  {
    val lst = List(2, 4, 10, 14, 7, 9, 3, 16, 8, 1)
    val g = (x:Option[Int]) => x.get
    it("should returns max element") {
      assert((buildPiramid(lst) |> max).get == 16)
    }
  }
  
  describe("extractMax that should extract max element from the heap") {
    import scala.Stream
    val lst = List(2, 4, 10, 14, 7, 9, 3, 16, 8, 1)
    val q = buildPiramid(lst)
    it("should produce a sorted list if n times apply extractMax to list") {
      def next(h: Heap[Int]): Stream[Option[Int]] = extractMax(h) match { case (a, b) => a #:: next(b) }
      val maxLs: List[Int] = (None #:: next(q) take lst.length + 1 toList) flatten
      val expect = lst sortWith (_ > _)
      assert(expect == maxLs)
    }
  }

  import Sorting.qSort._  
  describe("idiomatic Scala Quicksort") {
    import scala.Stream
    val lst = List(2, 4, 10, 14, 7, 9, 3, 16, 8, 1)
    it("should sort") {
      assert(qsort(lst) == lst.sorted)
    }
  }

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}