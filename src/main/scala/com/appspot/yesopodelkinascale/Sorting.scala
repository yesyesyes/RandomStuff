package com.appspot.yesopodelkinascale

object Sorting {

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