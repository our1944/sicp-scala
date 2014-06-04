package chapter1

import chapter1.ProcedureGenProcess._

import org.scalatest._

class ProcedureGenProcessSpec extends FlatSpec {

  // exercise 1.10
  "ackerman(1, 10)" should "compute 2^10" in {
    assert(ackermann(1, 10) == math.pow(2, 10))
  }

  "ackermann(2, 4)" should "compute 2^16" in {
    assert(ackermann(2, 4) == math.pow(2, 16))
  }

  "ackermann(3, 3)" should "compute 2^16" in {
    assert(ackermann(3, 3) == math.pow(2, 16))
  }

  // exercise 1.11
  "recurf and iterf" should "compute same value for 10" in {
    assert(recurf(10) == iterf(10))
  }

  // exercise 1.12
  "pascalTriVal" should "compute 6 for row 4 col 2 " in {
    assert(pascalTriVal(4, 2) == 6)
  }

  // exercise 1.16
  "fastExpt(2, 10) succExpt(2, 10)" should "compute 1024" in {
    assert(fastExpt(2, 10) == 1024)
    assert(succExpt(2, 10) == 1024)
  }

  // exercise 1.17
  "fastMulti(2, 5)" should "compute 10" in {
    assert(fastMulti(2, 5) == 10)
  }

  // exercise 1.18
  "russians" should "be clever" in {
    assert(russianPeasant(56, 551) == 30856)
    assert(russianPeasant(551, 56) == 30856)
    assert(russianPeasant(-551, 56) == -30856)
  }

  // exercise 1.19
  "fib exp" should "compute same resutl as old fib" in {
    assert(fibOrig(10) == fib(10))
  }

  // exercise 1.21
  "smallestDivisor" should "compute correct smallest divisor for 199 1999 and 19999" in {
    assert(smallestDivisor(199) == 199)
    assert(smallestDivisor(1999) == 1999)
    assert(smallestDivisor(19999) == 7)
  }

  // exercise 1.22
  // TODO: figure out a better way to test
  "searchForPrime" should "have some order of growth of O(sqrt(n))" in {
    val (result, time) = searchForPrime(1000, 0, 0, Nil, isPrime)
    val (result1, time1) = searchForPrime(10000, 0, 0, Nil, isPrime)
    val (result2, time2) = searchForPrime(100000, 0, 0, Nil, isPrime)
    val (result3, time3) = searchForPrime(10000000000L, 0, 0, Nil, isPrime)
    val (result4, time4) = searchForPrime(1000000000000L, 0, 0, Nil, isPrime)
    val (result5, time5) = searchForPrime(100000000000000L, 0, 0, Nil, isPrime)

    reportTime(result, time)

    reportTime(result1, time1)

    reportTime(result2, time2)

    reportTime(result3, time3)

    reportTime(result4, time4)

    reportTime(result5, time5)

  }

  // exercise 1.24
  // TODO: better way to test, figure out the problem of performance
  "searchForPrime with fastPrime performance" should "have O(n) = log(n)" in {
    val (result, time) = searchForPrime(100, 0, 0, Nil, fastPrime)
    val (result1, time1) = searchForPrime(10000, 0, 0, Nil, fastPrime)
    val (result2, time2) = searchForPrime(100000000, 0, 0, Nil, fastPrime)
    //val (result3, time3) = searchForPrime(10000000000L, 0, 0, Nil, fastPrime)

    reportTime(result, time)
    reportTime(result1, time1)
    reportTime(result2, time2)
    //reportTime(result3, time3)
  }

  // exercise 1.27
  "Carmichael numbers" should "fool femattest" in {
    assert(foolFematTest(561))
    assert(foolFematTest(1105))
    assert(foolFematTest(1729))
    assert(foolFematTest(2465))
    assert(foolFematTest(2821))
    assert(foolFematTest(6601))
  }

  "Carmichael numbers" should "not fool millter rabin test" in {
    assert(mrTest(561) == false)
    assert(mrTest(1105) == false)
    assert(mrTest(1729) == false)
    assert(mrTest(2465) == false)
    assert(mrTest(2821) == false)
    assert(mrTest(6601) == false)
  }
}
