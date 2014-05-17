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
}
