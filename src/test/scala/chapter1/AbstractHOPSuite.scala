package chapter1

import org.scalatest._
import chapter1.AbstractHOP._

class AbstractHOPSuite extends FlatSpec {

  "cube" should "be type generic" in {
    assert(cube(3) == 27)
    assert(cube(0.1) == 0.001)
  }
}
