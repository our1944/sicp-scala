package chapter1

import org.scalatest._
import chapter1.AbstractHOP._

class AbstractHOPSuite extends FlatSpec {

  "cube" should "be type generic" in {
    assert(cube(3) == 27)
    assert(cube(0.1) - 0.001 < 0.000000000000001)
  }

  "sumLinear" should "be type generic" in {
    assert(sumLinear(((x: Int) => x), 2, ((x: Int) => x + 1), 1) == 0)
    assert(sumLinear(((x: Double) => x), 2.0, ((x: Double) => x + 1), 1.0) == 0)
  }

}
