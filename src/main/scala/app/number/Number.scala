package app.number

trait Number[A] {
  val a: A
  val zero: A
  val one: A
  def +(b: A): A
  def -(b: A): A
  def *(b: A): A
  def /(b: A): A
  def < (b: A): Boolean
  def > (b: A): Boolean
  def <= (b: A): Boolean
  def >= (b: A): Boolean
  def unary_+ = a
  def unary_- : A
  def abs: A
  def sqrt: A
}

object Number {
  import scala.language.implicitConversions
  implicit def numDouble(x: Double) = new Number[Double] {
    override val a = x
    override val zero = 0.0
    override val one = 1.0
    override def +(b: Double) = a+b
    override def -(b: Double) = a-b
    override def *(b: Double) = a*b
    override def /(b: Double) = a/b
    override def < (b: Double) = a < b
    override def > (b: Double) = a > b
    override def <= (b: Double) = a <= b
    override def >= (b: Double) = a >= b
    override def sqrt =  math.sqrt(a)
    override def unary_- = zero - a
    override def abs = if (a < zero) -a else a
    override def toString = a.toString
  }

  implicit def numBigDecimal(x: BigDecimal) = new Number[BigDecimal] {
    override val a = x
    override val zero = BigDecimal(0)
    override val one = BigDecimal(1)
    override def +(b: BigDecimal) = a+b
    override def -(b: BigDecimal) = a-b
    override def *(b: BigDecimal) = a*b
    override def /(b: BigDecimal) = a/b
    def < (b: BigDecimal) = a < b
    def > (b: BigDecimal) = a > b
    def <= (b: BigDecimal) = a <= b
    def >= (b: BigDecimal) = a >= b
    override def sqrt = Number.sqrt(a)
    override def unary_- = zero - a
    override def abs = if (a < zero) -a else a
    override def toString = a.toString
  }

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
      if (x*x == a) BigDecimal(x) else result(50)
    } else result(sc)
  }

}