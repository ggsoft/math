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
    override def +(b: Double) = a + b
    override def -(b: Double) = a - b
    override def *(b: Double) = a * b
    override def /(b: Double) = a / b
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
    override def +(b: BigDecimal) = a + b
    override def -(b: BigDecimal) = a - b
    override def *(b: BigDecimal) = a * b
    override def /(b: BigDecimal) = a / b
    def < (b: BigDecimal) = a < b
    def > (b: BigDecimal) = a > b
    def <= (b: BigDecimal) = a <= b
    def >= (b: BigDecimal) = a >= b
    override def sqrt = app.number.sqrt(a)
    override def unary_- = zero - a
    override def abs = if (a < zero) -a else a
    override def toString = a.toString
  }

}