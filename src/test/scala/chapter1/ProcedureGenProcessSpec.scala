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
}
