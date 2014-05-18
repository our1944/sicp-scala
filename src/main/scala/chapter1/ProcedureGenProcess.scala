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

  // exercise 1.11
  def recurf(x: Int): Int = 
    if (x < 3)
      x
    else
      recurf(x -1) + 2 * recurf(x - 2) + 3 * recurf(x - 3)

  def iterfIter(x: Int, y: Int, z: Int, count: Int): Int = 
    if (count == 0)
      y
    else
      iterfIter(x + 2 * y + 3 * z, x, y, count -1)

  def iterf(x: Int) = if (x < 3) x else iterfIter(2, 1, 0, x - 1)

  // exercise 1.12 col & row start with 0
  def pascalTriVal(row: Int, col: Int): Int =
    if (col < 0 || col > row)
      throw new java.util.NoSuchElementException
    else if (col == 0)
      1
    else if (col == row)
      1
    else
      pascalTriVal(row -1, col) + pascalTriVal(row - 1, col -1)


}
