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

  "exercise 1.7 betterEnough" should "works better for big & small numbers" in {
    // for small number, goodEnough ends too early
    assert(sqrt(0.000001) > betterSqrt(0.000001))
    // TODO bigger example
  }

  "exercise 1.8" should "calculate cube root" in {
    assert(math.abs(cubeRoot(27.0) - 3.0) < 0.0001)
  }

}
