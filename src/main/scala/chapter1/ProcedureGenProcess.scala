package chapter1

object ProcedureGenProcess {

  // exercise 1.10
  def ackermann(x: Int, y: Int): Int = 
    if (y == 0)
      0
    else if (x == 0)
      2 * y
    else if (y == 1)
      2
    else
      ackermann((x - 1), ackermann(x, (y - 1)))
}
