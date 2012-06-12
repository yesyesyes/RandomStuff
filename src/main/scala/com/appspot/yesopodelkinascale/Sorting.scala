package com.appspot.yesopodelkinascale
import scala.annotation.tailrec

object Sorting {

 object insertion {
  def insertion[A <% Ordered[A]](arr: Array[A]): Array[A] = {
    for (j <- 1 until arr.length) {
      val key = arr(j)
      var i = j - 1
      while (i >= 0 && arr(i) > key) {
        arr(i + 1) = arr(i)
        i = i - 1
      }
      arr(i + 1) = key
    }
    arr
  }

  def funInsertion[A <% Ordered[A]](ls: List[A]) = {
    def insert[A <% Ordered[A]](el: A, ls: List[A]): List[A] = ls match {
      case Nil => el :: Nil
      case x :: xs =>
        if (el < x) el :: x :: xs
        else x :: insert(el, xs)
    }
    (List[A]() /: ls) { (a, b) => insert(b, a) }
  }
 }

object merge {
  
  def funMerge[A <% Ordered[A]](l: List[A], r: List[A]) = {
    def rec(l: List[A], r: List[A], res: List[A]): List[A] = (l, r) match {
      case (Nil, Nil) => res reverse
      case (Nil, y) => (y ++ res) reverse
      case (x, Nil) => (x ++ res) reverse
      case (x :: xs, y :: ys) => if (x <= y) rec(xs, y :: ys, x :: res) else rec(x :: xs, ys, y :: res)
    }
    rec(l, r, Nil)
  }
  
  def merge[A <% Ordered[A]: ClassManifest](arr: Array[A], p: Int, q: Int, r: Int): Array[A] = {
    val left = new Array[A](q - p + 1)
    val right = new Array[A](r - q)
    Array.copy(arr, p, left, 0, q - p + 1)
    Array.copy(arr, q + 1, right, 0, r - q)
    var i, j = 0
    for(k <- p to r) {
      if(i >= left.length) arr(k) = right(j)
      else if(j >= right.length) arr(k) = left(i)
      else if (left(i) <= right(j)) {
        arr(k) = left(i)
        i = i + 1
      } else {
        arr(k) = right(j)
        j = j + 1
      }
    }
    arr
  }
}

object heap {
  
  def parent(i: Int) = i / 2
  def left(i: Int) = 2 * i + 1
  def right(i: Int) = 2 * i + 2
  
  def maxHeapify[A <% Ordered[A]](arr: Array[A], i: Int, max: Int) {
    val l = left(i)
    val r = right(i)
    var largest = -1
    if (l < max && arr(l) > arr(i)) largest = l
    else largest = i
    if (r < max && arr(r) > arr(largest)) largest = r
    if (largest != i) {
      val tmp = arr(i)
      arr(i) = arr(largest)
      arr(largest) = tmp
      maxHeapify(arr, largest, max)
    }
  }
  
  def buildMaxHeap[A <% Ordered[A]](arr: Array[A]) {
    for (i <- (arr.size - 1) / 2 to 0 by -1) maxHeapify(arr, i, arr.size)
  }
  
  def heap[A <% Ordered[A]](arr: Array[A]) {
    buildMaxHeap(arr)
    var max = arr.size
    for (i <- arr.size - 1 to 1 by -1) {
      val tmp = arr(0)
      arr(0) = arr(i)
      arr(i) = tmp
      max -= 1
      maxHeapify(arr, 0, max)
    }
  }
  
  sealed abstract case class Heap[+A] { def rank: Int }
  case object E extends Heap[Nothing] { def rank = 0 }
  case class T[+A](rank: Int, x: A, a: Heap[A], b: Heap[A]) extends Heap[A]
  
  def mk[A](x: A, a: Heap[A], b: Heap[A]) = {
    if (a.rank > b.rank) T(b.rank + 1, x, a, b)
    else T(a.rank + 1, x, b, a)
  }
  
  @tailrec def toList[A <% Ordered[A]](a: Heap[A], res: List[A]): List[A] = a match {
    case E => res
    case t: T[A] => toList(merge(t.a, t.b), t.x :: res)
  } 
  
  def merge[A <% Ordered[A]](a: Heap[A], b: Heap[A]): Heap[A] = (a, b) match {
    case (E, h) => h
    case (h, E) => h
    case (h1 @ T(_, x, a1, b1), h2 @ T(_, y, a2, b2)) =>
      if (x >= y) mk(x, a1, merge(b1, h2))
      else mk(y, a2, merge(b2, h1))
  }
  
  def buildPiramid[A <% Ordered[A]](ls: List[A]): Heap[A] = ls.foldLeft(E: Heap[A]) { (x, y) => merge(mk(y, E, E), x) }
  
  def max[A](h: Heap[A]) = h match {
    case E => None
  	case x: T[A] => Some(x.x)
  }
  
  def extractMax[A <% Ordered[A]](h: Heap[A]): (Option[A], Heap[A]) = h match {
    case E => (None, E)
    case t: T[A] => max(t) -> merge(t.a, t.b)
  }
  
  def heapSort[A <% Ordered[A]](xs: List[A]) = toList(xs.foldLeft(E: Heap[A]) { (x, y) => merge(mk(y, E, E), x) }, Nil)

}

  object qSort {
    //source: http://stackoverflow.com/a/2962799
    def qsort[A: Ordering](ls: List[A]) = {
      import Ordered._
      def sort(ls: List[A])(parent: List[A]): List[A] = {
        if (ls.size <= 1) ls ::: parent else {
          val pivot = ls.head
          val (less, equal, greater) = ls.foldLeft((List[A](), List[A](), List[A]())) {
            case ((less, equal, greater), e) => {
              if (e < pivot)
                (e :: less, equal, greater)
              else if (e == pivot)
                (less, e :: equal, greater)
              else
                (less, equal, e :: greater)
            }
          }
          sort(less)(equal ::: sort(greater)(parent))
        }
      }
      sort(ls)(Nil)
    }
  }
}