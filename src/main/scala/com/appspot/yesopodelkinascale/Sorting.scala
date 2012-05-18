package com.appspot.yesopodelkinascale

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable.Queue

object Sorting extends FunSuite with ShouldMatchers {

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