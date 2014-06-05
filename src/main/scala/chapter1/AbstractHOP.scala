package chapter1

import Numeric.Implicits._

object AbstractHOP {
  // magic of implict function in companion object Numeric
  def cube[T: Numeric](x: T) = x * x * x

  def sumLinear[T: Numeric](term: T => T, a: T, next: T => T, b: T): T =
    if (implicitly[Numeric[T]].gt(a, b)) implicitly[Numeric[T]].zero // could be improved
    else term(a) + sumLinear(term, next(a), next, b)

  // type error ...
  def integral[T: Numeric](f: T => T, a: T, b: T, dx: T) = {
    def addDx(x: T) = x + dx
    sumLinear(f, a + dx / 0.2, addDx, b) * dx
  }
}
