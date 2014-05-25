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

  // exercise 1.16
  def square(x: Int) = x * x

  def fastExpt(x: Int, y: Int): Int =
    if (y == 0) 1
    else if (y % 2 == 0) square(fastExpt(x, y / 2))
    else x * fastExpt(x, y - 1)

  def succExptIter(b: Int, n: Int, a: Int): Int =
    if (n==0)
      a
    else if (n % 2 == 0) succExptIter(square(b), n/2, a)
    else succExptIter(b, n - 1, a * b)

  def succExpt(x: Int, y: Int): Int = succExptIter(x, y, 1)

  def fastMulti(x: Int, y: Int): Int =
    if (y == 0) 0
    else if (y == 1) x
    else if (y % 2 == 0) 2 * fastMulti(x, y / 2)
    else x + fastMulti(x, y - 1)

  // exercise 1.18
  def russianPeasant(x: Int, y: Int): Int =
    if (x == 0 || y == 0) 0
    else if (x < 0) russianPeasantIter(-x, -y, 0)
    else russianPeasantIter(x, y, 0)

  def russianPeasantIter(x: Int, y: Int, z:Int): Int =
    if (x == 0) z
    else if (x % 2 == 0) russianPeasantIter(x / 2, y * 2, z)
    else russianPeasantIter((x - 1) / 2, y * 2, z + y)

  // exercise 1.19
  // Tpq
  // a' = bq + aq + ap
  //    = (q + p)a + bq
  // b' = bp + aq
  //
  // (Tpq)^2
  // a'' = b'q + a'q + a'p
  //     = (bpq + aq^2) + (bq^2 + aq^2 + apq) + (bqp + aqp + ap^2)
  //     = (2q^2 + 2pq + p^2)a + (q^2 + 2pq)b
  //
  // b'' = b'p + a'q
  //     = (bp^2 + aqp) + (bq^2 + aq^2 + apq)
  //     = (p^2 + q^2)b + (q^2 + 2pq)a
  //
  //
  // Tp'q'
  // a'' = bq' + aq' + ap'
  //     = (p' + q')a + bq'
  //
  // b'' = bp' + aq'
  //
  // q' = q^2 + p^2
  // p' = 2pq + q^2

  def fib(n: Int): Int = {
    def fibIter(a:Int, b:Int, p: Int, q: Int, count: Int): Int =
      if (count == 0) b
      else if (count % 2 == 0)
        fibIter(a, b, p * p + q * q, q * q + 2 * p * q, count / 2)
      else fibIter(b * q + a * q + a * p, b * p + a * q, p, q, count - 1)
    fibIter(1, 0, 0, 1, n)
  }

  def fibOrig(n: Int): Int =
    if (n == 0) 0
    else if (n == 1) 1
    else fibOrig(n - 1) + fibOrig(n - 2)
}
