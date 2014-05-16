package chapter1

import chapter1.ElmOfProgram._
import org.scalatest._

class ElmOfProgramSpec extends FlatSpec {

  "exercise 1.3: sumSquareGreaterTwo" should "take 3 params and use square of bigger two to do sum" in {
    assert(sumSquareGreaterTwo(0, 3, 2) == 13)
    assert(sumSquareGreaterTwo(-1, -1, -1) == 2)
  }

  "exercise 1.6 newIf" should "get stack oveflow" in {
    intercept[java.lang.StackOverflowError] {
      newSqrt(2)
    }
  }

}
