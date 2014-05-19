package  chapter1

object ElmOfProgram {

  def square(x: Double) = x * x

  // exercise 1.3
  def sumSquareGreaterTwo(x: Int, y: Int, z:Int): Double =
    if (x <= y && x <= z)
      square(y) + square(z)
    else if (y <= x && y <= z)
      square(x) + square(z)
    else
      square(y) + square(x)

  // exercsie 1.6
  def newIf(pre: Boolean, then: Double, other: Double): Double =
    if (pre)
      then
    else
      other

  def average(x:Double, y:Double) = (x + y) / 2

  def goodEnough(guess: Double, x: Double): Boolean =
    if (math.abs((square(guess) - x)) < 0.001)
      true
    else
      false

  def improve(guess: Double, x:Double) = average(guess, x / guess)

  def sqrtIter(guess: Double, x:Double): Double =
    if (goodEnough(guess, x))
      guess
    else
      sqrtIter(improve(guess, x), x)


  def sqrt(x: Double) = sqrtIter(1.0, x)

  def newSqrtIter(guess: Double, x: Double): Double =
    newIf(goodEnough(guess, x), guess, newSqrtIter(improve(guess, x), x))

  def newSqrt(x: Double) = newSqrtIter(1.0, x)

  // exercise 1.7
  def betterEnough(guess: Double, lastGuess: Double, x: Double) =
    if (math.abs(guess / lastGuess - 1) < 0.00001)
      true
    else
      false

  def betterSqrtIter(guess: Double, lastGuess: Double, x: Double): Double =
    if (betterEnough(guess, lastGuess, x))
      guess
    else
      betterSqrtIter(improve(guess, x), guess, x)

  def betterSqrt(x: Double) = betterSqrtIter(1.0, 1.5, x)

  // exercise 1.8
  def improveCube(guess: Double, x: Double): Double = ((x / square(guess)) + 2.0 * guess) / 3.0

  def cubeRootIter(guess: Double, lastGuess: Double, x: Double): Double =
    if (betterEnough(guess, lastGuess, x))
      guess
    else
      cubeRootIter(improveCube(guess, x), guess, x)

  def cubeRoot(x: Double): Double = cubeRootIter(3.0, 1.5, x)

}
