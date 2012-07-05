package com.appspot.yesopodelkinascale
import org.junit.runner.RunWith
import org.scalacheck.Prop._
import org.scalatest.junit.JUnitRunner
import org.scalatest.junit.JUnitSuite
import org.scalatest.prop.Checkers
import org.scalatest.FunSpec

@RunWith(classOf[JUnitRunner])
class TypesTest extends FunSpec with Checkers {
  
  import Category._
  describe("A Category") {
    
    val f = (i: Int) => i.toString
    val g = (s: String) => s.length
    val h = (i: Int) => i * i
    
    it("should satisfy identity: f ο 1A = f and 1B ο f = f") {
      forAll { i: Int => 
        compose(id[Int], f)(i) == compose(f, id[String])(i)
        } check
    }
    
    it("should satisfy associativity: f ο (g ο h) = (f ο g) ο h") {
      forAll { i: Int =>
        compose(f, compose(g, h))(i) == compose(compose(f, g), h)(i) 
      } check
    }
  }
  
  import Functor._
  describe("For ListFunctor") {
    val ls = List(1, 2, 3)
    it("fmap == map") {
      check { fmap(ls)(_ + 1) == ls.map(_ + 1) }  
    }
  }
  
  import ListFunctor.{fmap => lfmap}
  describe("A ListFunctor") {
    
    it("should preserve identity: F(1A) = 1F(A) ∀ A ∈ C1") {
      val stringId = (s: String) => s
      val stringListId = (ls: List[String]) => ls
      forAll { (ls: List[String]) => true
        lfmap(stringId)(ls) == stringListId(ls)
      } check
    }
      
    it("should preserve composition: F(f ο g) = F(g) ο F(g) ∀ f: A → B, g: B → C where A, B, C ∈ C1") {
      val g = (i: Int) => i toString
      val f = (s: String) => s length()
      forAll { (ls: List[Int]) =>
        lfmap(f compose g)(ls) == (lfmap(f) compose lfmap(g))(ls)
      } check
    }
  }
}