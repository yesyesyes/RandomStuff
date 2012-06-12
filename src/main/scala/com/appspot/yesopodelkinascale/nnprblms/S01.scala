package scalaproblems

import annotation.tailrec

/**
 * User: yesyesyes
 * Date: 07.01.12
 */

/**
 * Find the last element of a list.
 *
 * Example:
 *    scala> last(List(1, 1, 2, 3, 5, 8))
 *    res0: Int = 8
 */
object S01 {

  def main(args: Array[String]) {
    val ls = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(last(ls))
    println(last(ls))
  }
  
  def last2[A](ls: List[A]): A = ls reduceLeft { (x,y) => y }

  def last[T](ls: List[T]): T = {
    @tailrec def last(ls: List[T]): T = ls match {
      case x :: xs => if (xs.size == 1) xs(0) else last(xs)
    }
    last(ls)
  }

  @tailrec def last1[T](ls: List[T]): T = ls match {
    case Nil => throw new NoSuchElementException
    case x :: Nil => x
    case x :: xs => last1(xs)
  }

}

/**
 * Find the last but one element of a list.
 * Example:
 *    scala> penultimate(List(1, 1, 2, 3, 5, 8))
 *    res0: Int = 5
 */
object S02 {

  def main(args: Array[String]) {
    val ls = List(1, 1, 2, 3, 5, 8)
    println(penultimate(ls))
  }
  
  @tailrec def penultimate[A](ls: List[A]): A = ls match {
    case x :: y :: Nil => x
    case x :: y :: z => penultimate(z)
    case _ => throw new NoSuchElementException
  }

}

/**
 * Find the Kth element of a list.
 * By convention, the first element in the list is element 0.
 * Example:
 *    scala> nth(2, List(1, 1, 2, 3, 5, 8))
 *    res0: Int = 2
 */
object S03 {

  def main(args: Array[String]) {
    println(nth(2, List(1, 1, 2, 3, 5, 8)))
  }

  def nth[A](n: Int, ls: List[A]): A = {
    @tailrec def nthrec(cur: Int, curls: List[A]): A = ls match {
      case Nil if cur > 0 => throw new NoSuchElementException
      case x :: _ if cur == 0 => x
      case _ :: xs => nthrec(cur - 1, curls.tail)
    }
    if (n < 0) throw new IllegalArgumentException
    nthrec(n, ls)
  }

}

/**
 * Find the number of elements of a list.
 * Example:
 *     scala> length(List(1, 1, 2, 3, 5, 8))
 *     res0: Int = 6
 */
object S04 {
  def main(args: Array[String]) {
    println(length(List(1, 1, 2, 3, 5, 8)))
    println(length1(List(1, 1, 2, 3, 5, 8)))
  }

  def length[A](ls: List[A]): Int = {
    @tailrec def rec(ls: List[A], n: Int): Int = ls match {
      case Nil => n
      case x :: xs => rec(xs, n + 1)
    }
    rec(ls, 0)
  }

  def length1[A](ls: List[A]): Int = (0 /: ls) { (c, _) => c + 1 }
}

/**
 * Reverse a list.
 * Example:
 *    scala> reverse(List(1, 1, 2, 3, 5, 8))
 *    res0: List[Int] = List(8, 5, 3, 2, 1, 1)
 */
object S05 {
  def main(args: Array[String]) {
    val ls = List(1, 1, 2, 3, 5, 8)
    println(reverse(ls))
    println(reverse1(ls))
    println(rev1(ls))
    println(rev2(ls))
  }
  
  def scanl[A, B](ls: List[A], z: List[B], op: (List[B], A) => List[B]): List[B] = {
    var result = z
    ls foreach(x => result = op(result, x))
    result
  } 
  
  def reverse[A](ls: List[A]): List[A] = {
    @tailrec def rec(ls: List[A], res: List[A]): List[A] = ls match {
      case Nil => res
      case x :: xs => rec(xs, List(x) ++ res)
    }
    rec(ls, Nil)
  }
  
  def flip[A, B, C](f: A => B => C)(x: B)(y: A) = f(y)(x)
  def flip1[A, B, C](f: A => B => C): B => A => C = x => y => f(y)(x)  
  
  def rev2[A](ls: List[A]): List[A] = scanl(ls, List[A](), (x: List[A], y: A) => y :: x )
  def rev1[A](ls: List[A]): List[A] = (List[A]() /: ls) { (x, y) =>
    flip { Function curried { (a: List[A], b: A) => b :: a }} (y)(x) }
  def reverse1[A](ls: List[A]): List[A] = (List[A]() /: ls) { (f, g) => g :: f }
}

/**
 * Find out whether a list is a palindrome.
 * Example:
 * 	  scala> isPalindrome(List(1, 2, 3, 2, 1))
 *    res0: Boolean = true
 */
object S06 {
  def main(args: Array[String]) {
    val ls = List(1, 2, 3, 2, 1)
    println(isPalindrome(ls))
    println(isPal(ls))
  }
  
  @tailrec def isPalindrome[A](ls: List[A]): Boolean = ls match {
    case Nil => true
    case x :: Nil => true
    case x :: xs => if (x.equals(xs.last)) isPalindrome(xs.init) else false
  }

  def isPal[A](ls: List[A]): Boolean = ls.take(ls.length / 2 + ls.length % 2) == ls.drop(ls.length / 2).reverse
}

/**
 * Flatten a nested list structure.
 * Example:
 * 		scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
 * 		res0: List[Any] = List(1, 1, 2, 3, 5, 8)
 */
object S07 {
  def main(args: Array[String]) {
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  }

  //TODO tailrec
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case l: List[_] => flatten(l)
    case x => List(x)
  }
}

/**
 * Eliminate consecutive duplicates of list elements.
 * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
 * Example:
 * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
 */
object S08 {
  def main(args: Array[String]) {
    println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
  def compress[A](ls: List[A]): List[A] = {
    @tailrec def compressrec(ls: List[A], res: List[A]): List[A] = ls match {
      case Nil => res.reverse
      case x :: Nil => (x :: res).reverse
      case x :: xs => if (x == xs.head) compressrec(xs, res) else compressrec(xs, x :: res)
    }
    compressrec(ls, Nil)
  }
}

/**
 * Pack consecutive duplicates of list elements into sublists.
 * If a list contains repeated elements they should be placed in separate sublists.
 *
 * Example:
 *
 * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
 */
object S09 {
  def main(args: Array[String]) {
    println(pack1(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
  def pack[A](ls: List[A]): List[List[A]] = {
    @tailrec def packrec(ls: List[A], res: List[List[A]]): List[List[A]] = ls match {
      case Nil => res.reverse
      case x :: xs =>
        packrec(xs,
          if (res != Nil)
            if (res.head(0) == x) (x :: res.head) :: res.tail
          else List(x) :: res
          else List(x) :: res)
    }
    packrec(ls, Nil)
  }
  def pack1[A](ls: List[A]): List[List[A]] = {
    @tailrec def packrec(ls: List[A], res: List[List[A]]): List[List[A]] = ls match {
      case Nil => res.reverse
      case x :: xs => packrec(xs.dropWhile(_ == x), (x :: xs).takeWhile(_ == x) :: res)
    }
    packrec(ls, Nil)
  }
}

/**
 * Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.

	Example:

	scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 */
object S10 {
  def main(args: Array[String]) {
    println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
  def encode[A](ls: List[A]): List[(Int, A)] = S09 pack1 ls map { x => (x length, x first) }
}

/**
 * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.

	Example:

	scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 */
object S11 {
  def main(args: Array[String]) {
    println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))  
  }
  def encodeModified[A](ls: List[A]): List[Any] = S10 encode ls map { x => if (x._1 == 1) x._2 else x }
}

/**
 * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.

	Example:

	scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
	res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
 */
object S12 {
 def main(args: Array[String]) {
   println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
 }
 def decode[A](ls: List[(Int, A)]): List[A] = (ls map { x => List.fill(x._1)(x._2) }).flatten
}

/**
 * Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.

	Example:

	scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 */
object S13 {
  def main(args: Array[String]) {
    println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    @tailrec def enctailrec(ls: List[A], res: List[(Int, A)]): List[(Int, A)] = ls match {
      case Nil => res.reverse
      case x => val t = x.span(_ == x.head)
      println(t)
        enctailrec(t._2, (t._1.length, x.head) :: res)
    }
    enctailrec(ls, Nil)
  }
}

/**
 * Example:

	scala> duplicate(List('a, 'b, 'c, 'c, 'd))
	res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
 */
object S14 {
  def main(args: Array[String]) {
    println(duplicate(List('a, 'b, 'c, 'c, 'd)))
  }
  def duplicate[A](ls: List[A]): List[A] = {
    @tailrec def dupltailrec(ls: List[A], res: List[A]): List[A] = ls match {
      case Nil => res.reverse
      case x :: xs => dupltailrec(xs, x :: x :: res)
    }
    dupltailrec(ls, Nil)
  }  
}

/**
 * Example:

	scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
	res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
 */
object S15 {
  def main(args: Array[String]) {
    println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
  }
  def duplicateN[A](num: Int, ls: List[A]): List[A] = {
    def duplNrec(ls: List[A], res: List[List[A]]): List[A] = ls match {
      case Nil => res.flatten reverse
      case x :: xs => duplNrec(xs, List.fill(num)(x) :: res)
    }
    duplNrec(ls, Nil)
  }
}

/**
*Drop every Nth element from a list.
    Example:

    scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
*/
object S16 {
  def main(args: Array[String]) {
    println(drop1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
  def drop[A](n: Int, ls: List[A]): List[A] =  {
    @tailrec def droprec(ls: List[A], res: List[A]): List[A] = ls match {
      case Nil => res.reverse
      case x => val r = x.splitAt(n - 1)
      droprec(if (r._2 == Nil) Nil else r._2.tail, r._1.reverse ++ res) 
    }
    droprec(ls, Nil)
  }
  def drop1[A](n: Int, ls: List[A]): List[A] =
    ls.head :: ls.zipWithIndex.tail.filter(x => (x._2 + 1) % n != 0).map(x => x._1)
}

/**
*Split a list into two parts.
    The length of the first part is given. Use a Tuple for your result.

    Example:

    scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
*/
object S17 {
  def main(args: Array[String]) {
    println(split1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = (ls.take(n), ls.drop(n))
  def split1[A](n: Int, ls: List[A]): (List[A], List[A]) = ls.splitAt(n)
}

/**
*Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.

    Example:

    scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('d, 'e, 'f, 'g)
*/
object S18{
  def main(args: Array[String]) {
    val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    println(slice(3, 7, ls))
    println(slice1(3, 7, ls))
  }
  def slice[A](from: Int, to: Int, ls: List[A]): List[A] = ls take to drop from
  def slice1[A](from: Int, to: Int, ls: List[A]): List[A] = ls drop from take (to - from)
}

/**
*Rotate a list N places to the left.
    Examples:

    scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

    scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
*/
object S19{
  def main(args: Array[String]) {
    val ls = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k) 
    println(rotate(3, ls))
    println(rotate(-2, ls))
    
    println(rot(3, ls))
    println(rot(-2, ls))
  }
  
  def rot[A](n: Int, ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case xs if n == 0 => xs
    case x :: xs if n > 0 => rot(n - 1, xs ++ List(x))
    case xs if n < 0 => rot(n + 1, xs.last :: xs.init)
  }
  
  def rotate[A](n: Int, ls: List[A]): List[A] = 
    if (n == 0) ls else if (n > ls.length) throw new IllegalArgumentException 
    else if (n > 0) ls.drop(n) ::: ls.take(n) else ls.takeRight(-n) ::: ls.dropRight(-n)
}

/**
*Remove the Kth element from a list.
    Return the list and the removed element in a Tuple. Elements are numbered from 0.

    Example:

    scala> removeAt(1, List('a, 'b, 'c, 'd))
    res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
*/
object S20{
  def main(args: Array[String]) {
    val ls = List('a, 'b, 'c, 'd)
    println(removeAt(1, ls))
  }
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = 
    (ls.take(n) ++ ls.takeRight(ls.length - n).tail, ls(n))
}

/**
* Insert an element at a given position into a list.
    Example:

    scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
    res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
*/
object S21{
  def main(args: Array[String]) {
    val ls = List('a, 'b, 'c, 'd)
    println( insertAt('new, 1, ls) )
    println( ins('new, 1, ls) )
  }
  def ins[A](a: A, n: Int, ls: List[A]): List[A] = ls splitAt n match {
    case (x, y) => x ::: a :: y
  }
  
  def insertAt[A](a: A, n: Int, ls: List[A]): List[A] = 
    ls.take(n) ++ List(a) ++ ls.takeRight(ls.length - n) 
}

/**
* Create a list containing all integers within a given range.
    Example:

    scala> range(4, 9)
    res0: List[Int] = List(4, 5, 6, 7, 8, 9)
*/
object S22{
  import scala.collection.immutable.Stream
  def main(args: Array[String]) {
    println( range(4, 9) )
    println( range1(4, 8) )
  }
  def range(from: Int, to: Int): List[Int] = (from to to).toList
  def range1(from: Int, to: Int): List[Int] = {
    def r(n: Int): Stream[Int] = n #:: r(n + 1)
     r(from).take(to - from + 1).toList
  }
}


/**
*P23 (**) Extract a given number of randomly selected elements from a list.
    Example:

    scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    res0: List[Symbol] = List('e, 'd, 'a)

    Hint: Use the solution to problem P20
*/
object P23{
  def main(args: Array[String]) {
    println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
  }
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    @tailrec def r(n: Int, res: List[A]): List[A] = 
      if (n == 0) res 
      else r(n - 1, ls((Math.random * ls.length).asInstanceOf[Int]) :: res)
    r(n, Nil)
  }
}


/**
*P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    Example:

    scala> lotto(6, 49)
    res0: List[Int] = List(23, 1, 17, 33, 21, 37)
*/
object P24{
  def main(args: Array[String]) {
    println(lotto(6, 49))
  }
  def lotto(n: Int, to: Int): List[Int] = {
    @tailrec def r(n: Int, res: List[Int]): List[Int] = 
      if (n == 0) res
      else {
        val newNum = (Math.random * to).asInstanceOf[Int]
        if (res contains newNum) r(n, res) else r(n - 1, newNum :: res)
      }
    r(n, Nil)
  }
}

/**
*P25 (*) Generate a random permutation of the elements of a list.
    Hint: Use the solution of problem P23.

    Example:

    scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
*/
object P25{
  def main(args: Array[String]) {
    println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
  }
  import P23._
  def randomPermute[A](ls: List[A]): List[A] = {
    def r(ls1: List[A], res: List[A]):List[A] = 
      if (res.length == ls.length) res
      else {
        val e = randomSelect(1, ls1).first
        r( ls1.diff(List(e)), e :: res )    
      }
   r(ls, Nil) 
  } 
}

/**
*P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
    In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.

    Example:

    scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
    res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
*/
object P26{
  def main(args: Array[String]) {
    println(combinations(3, List(1, 2, 3, 4)))
  }
  
  //naive without tailrec
  def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
    def comb(k: Int, last: Int, res: List[A]): Seq[List[A]] = 
      if (k == 0) List(res)
      else (0 to ls.length - k).filter(_ > last).flatMap(x => comb(k - 1, x, ls(x) :: res))
      comb(n, -1, Nil).toList
  }
}







































