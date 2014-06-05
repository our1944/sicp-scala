package chapter1

import util.Random._


object ProcedureGenProcess {

  // exercise 1.10
  def ackermann(x: Int, y: Int): Int =
    if (y == 0)
      0
    else if (x == 0)
      2 * y
    else if (y == 1)
      2
    else
      ackermann((x - 1), ackermann(x, (y - 1)))

  // exercise 1.11
  def recurf(x: Int): Int =
    if (x < 3)
      x
    else
      recurf(x -1) + 2 * recurf(x - 2) + 3 * recurf(x - 3)

  def iterfIter(x: Int, y: Int, z: Int, count: Int): Int =
    if (count == 0)
      y
    else
      iterfIter(x + 2 * y + 3 * z, x, y, count -1)

  def iterf(x: Int) = if (x < 3) x else iterfIter(2, 1, 0, x - 1)

  // exercise 1.12 col & row start with 0
  def pascalTriVal(row: Int, col: Int): Int =
    if (col < 0 || col > row)
      throw new java.util.NoSuchElementException
    else if (col == 0)
      1
    else if (col == row)
      1
    else
      pascalTriVal(row -1, col) + pascalTriVal(row - 1, col -1)

  // exercise 1.16
  def square(x: Int)  = x * x

  def fastExpt(x: Int, y: Int): Int =
    if (y == 0) 1
    else if (y % 2 == 0) square(fastExpt(x, y / 2))
    else x * fastExpt(x, y - 1)

  def succExptIter(b: Int, n: Int, a: Int): Int =
    if (n==0)
      a
    else if (n % 2 == 0) succExptIter(square(b), n/2, a)
    else succExptIter(b, n - 1, a * b)

  def succExpt(x: Int, y: Int): Int = succExptIter(x, y, 1)

  def fastMulti(x: Int, y: Int): Int =
    if (y == 0) 0
    else if (y == 1) x
    else if (y % 2 == 0) 2 * fastMulti(x, y / 2)
    else x + fastMulti(x, y - 1)

  // exercise 1.18
  def russianPeasant(x: Int, y: Int): Int =
    if (x == 0 || y == 0) 0
    else if (x < 0) russianPeasantIter(-x, -y, 0)
    else russianPeasantIter(x, y, 0)

  def russianPeasantIter(x: Int, y: Int, z:Int): Int =
    if (x == 0) z
    else if (x % 2 == 0) russianPeasantIter(x / 2, y * 2, z)
    else russianPeasantIter((x - 1) / 2, y * 2, z + y)

  // exercise 1.19
  // Tpq
  // a' = bq + aq + ap
  //    = (q + p)a + bq
  // b' = bp + aq
  //
  // (Tpq)^2
  // a'' = b'q + a'q + a'p
  //     = (bpq + aq^2) + (bq^2 + aq^2 + apq) + (bqp + aqp + ap^2)
  //     = (2q^2 + 2pq + p^2)a + (q^2 + 2pq)b
  //
  // b'' = b'p + a'q
  //     = (bp^2 + aqp) + (bq^2 + aq^2 + apq)
  //     = (p^2 + q^2)b + (q^2 + 2pq)a
  //
  //
  // Tp'q'
  // a'' = bq' + aq' + ap'
  //     = (p' + q')a + bq'
  //
  // b'' = bp' + aq'
  //
  // q' = q^2 + p^2
  // p' = 2pq + q^2

  def fib(n: Int): Int = {
    def fibIter(a:Int, b:Int, p: Int, q: Int, count: Int): Int =
      if (count == 0) b
      else if (count % 2 == 0)
        fibIter(a, b, p * p + q * q, q * q + 2 * p * q, count / 2)
      else fibIter(b * q + a * q + a * p, b * p + a * q, p, q, count - 1)
    fibIter(1, 0, 0, 1, n)
  }

  def fibOrig(n: Int): Int =
    if (n == 0) 0
    else if (n == 1) 1
    else fibOrig(n - 1) + fibOrig(n - 2)

  def squareLong(x: Long) = x * x

  def smallestDivisor(n: Long, useNext: Boolean = false): Long = {
    def next(y: Long): Long =
      if (y == 2) 3
      else y + 2

    def findDivisor(x: Long, test: Long): Long =
      if ((test * test) > n) n
      else if (x % test == 0) test
      else if (!useNext) findDivisor(x, test + 1)
      else findDivisor(x, next(test))

    findDivisor(n, 2)
  }

  def expmod(base: Long, exp: Long, m: Long): Long =
    if (exp == 0) 1
    else if (exp % 2 == 0)
      squareLong(expmod(base, exp / 2, m)) % m
    else (base * expmod(base, exp - 1, m)) % m


  def isPrime(n: Long) = n == smallestDivisor(n)

  def rand(l: Long, u: Long): Long = l + (nextDouble() * (u - l)).asInstanceOf[Long]

  def fematTest(n: Long): Boolean = {
    def tryIt(a: Long): Boolean = a == expmod(a, n, n)
    tryIt(rand(1, n - 1))
  }

  def fastPrime(n: Long): Boolean = {
    def fastPrimeIter(times: Int): Boolean =
      if (times == 0) true
      else if (fematTest(n)) fastPrimeIter(times -1)
      else false
    fastPrimeIter(100)
  }

  def timedPrimeTest(n: Long, f: Long => Boolean): (Boolean, Long) = {
    val starttime = System.currentTimeMillis
    if (f(n)) (true, System.currentTimeMillis - starttime)
    else (false, System.currentTimeMillis - starttime)
  }

  def reportTime(result: List[Long], elapsed: Long) = println(result + " *** " + elapsed)

  def searchForPrime(bottom: Long, count: Int, timeAcc: Long, primeAcc: List[Long], f: Long => Boolean): (List[Long], Long) = {
    val testResult = timedPrimeTest(bottom + 1, f)
    //println("count " + count + " bottom " + bottom)
    if (count > 2) (primeAcc, timeAcc)
    else if (testResult._1) searchForPrime(bottom + 1, count + 1, timeAcc + testResult._2, primeAcc :+ (bottom + 1), f)
    else searchForPrime(bottom + 1, count, timeAcc + testResult._2, primeAcc, f)
  }


  // exercise 1.26
  // expmod evaluated twice

  // exercise 1.27
  def foolFematTest(n: Int): Boolean = {
    def foolFematTestIter(x: Int): Boolean =
      if (x == n) true
      else if (expmod(x, n, n) == x) foolFematTestIter(x + 1)
      else false
    foolFematTestIter(1)
  }

  // exercise 1.28
  def mrExpmod(base: Long, exp: Long, m: Long): Long = {
    def squaremodWithCheck(x: Long): Long = {
      def checkNontrivalSqrt1(x: Long, square: Long): Long =
        if (square == 1 && x != 1 && x != m - 1) 0
        else square
      checkNontrivalSqrt1(x, squareLong(x) % m)
    }
    if (exp == 0) 1
    else if (exp % 2 == 0)
      squaremodWithCheck(mrExpmod(base, exp / 2, m))
    else (base * mrExpmod(base, exp - 1, m)) % m
  }

 def mrTest(n: Long): Boolean = {
   def tryIt(a: Long): Boolean = {
     def checkIt(x: Long) = x != 0 && x == 1
     checkIt(mrExpmod(a, n - 1, n))
   }
   tryIt(rand(1, n - 1))
 }

}
