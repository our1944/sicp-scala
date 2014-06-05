package chapter1

import Numeric.Implicits._

object AbstractHOP {
  def cube[T: Numeric](x: T) = x * x * x
}
