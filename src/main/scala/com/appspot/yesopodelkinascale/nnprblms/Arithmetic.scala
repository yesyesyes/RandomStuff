package scalaproblems

/**
 * Determine whether a given integer number is prime.

    scala> 7.isPrime
    res0: Boolean = true
 */
object P31 {
  def main(args: Array[String]) {
    println(8.isPrime)
  }
  implicit def intToPrimeTest(i: Int): PrimeTest = new PrimeTest(i)
  case class PrimeTest(val num: Int) {
    def isPrime: Boolean = (2 until Math.sqrt(num).asInstanceOf[Int]).forall(num % _ != 0) 
  }
}

/**
 * Determine the greatest common divisor of two positive integer numbers.
    Use Euclid's algorithm.

    scala> gcd(36, 63)
    res0: Int = 9
 */
object P32 {
  def main(args: Array[String]) {
    println(gcd(36, 63))
  }
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b) 
}

/**
* Determine whether two positive integer numbers are coprime.
    Two numbers are coprime if their greatest common divisor equals 1.

    scala> 35.isCoprimeTo(64)
    res0: Boolean = true
*/
object P33{
  def main(args: Array[String]) {
    println( 35.isCoprimeTo(64) )
  }
  case class CoprimeTest(i: Int) {
    import P32._
    def isCoprimeTo(n: Int) = gcd(i, n) == 1 
  }
  implicit def intToCoprimeTest(i: Int): CoprimeTest = CoprimeTest(i)
}

/**
* Calculate Euler's totient function phi(m).
    Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.

    scala> 10.totient
    res0: Int = 4
*/
object P34{
  def main(args: Array[String]) {
    println(10.totient)
  }
  case class TotientTest(i: Int) {
    import P33._
    def totient = (1 to i) filter (_.isCoprimeTo(i)) length
  }
  implicit def intToTotientTest(n: Int): TotientTest = TotientTest(n)  
}

