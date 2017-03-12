package app

package object number {

  private val accuracy = 80 // Number of digits in fractional part

  def sqrt(a: BigDecimal, scale: Int = 0): BigDecimal = {

    def sqrt(a: BigInt): BigInt = { // find b: BigInt => b^2 <= a < (b + 1)^2
      if (a < 0) throw new ArithmeticException("Square root of negative number")
      else if (a < 2) a
      else {
        def next(n: BigInt, i: BigInt): BigInt = (n + i/n) >> 1
        var n0 = BigInt(2).pow((a.bitLength >> 1) + (a.bitLength & 1))
        var n1 = next(n0, a)
        while ((n1 - n0).abs > 1) {
          n0 = n1
          n1 = next(n0, a)
        }
        while (n1 * n1 > a) {
          n1 -= 1
        }
        n1
      }
    }

    def result(sc: Int) = {
      val p = BigDecimal("1"+"0"*(sc*2))
      val q = BigDecimal("1"+"0"*sc)
      BigDecimal(sqrt((a * p).toBigInt)).setScale(sc)/q
    }

    val sc = math.max(a.scale, scale)
    if (sc == 0) {
      val x = sqrt(a.toBigInt)
      if (x*x == a) BigDecimal(x) else result(accuracy)
    } else result(sc)
  }

}
