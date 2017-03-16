package app.number

case class Complex(x: BigDecimal, y: BigDecimal) {
  private val d = x*x + y*y
  val mod = sqrt(d)
  val zero = Complex.zero
  val one = Complex.one
  def unary_~ = Complex(x, -y)
  def unary_- = Complex(-x, -y)
  def unary_+ = this
  def inv = ~Complex(x/d, y/d) // inverse number
  def +(z: Complex) = Complex(x + z.x, y + z.y)
  def -(z: Complex) = Complex(x - z.x, y - z.y)
  def *(z: Complex) = Complex(x*z.x - y*z.y, x*z.y + y*z.x)
  def /(z: Complex) = this * z.inv

  def pow(n: Int): Complex = { // power
    if (n > 0) this * (pow(n-1))
    else if (n < 0) Complex(one, zero) / (pow(-n))
    else Complex(one, zero)
  }

  override def toString = {
    if ((x == zero) && (y == zero)) "0"
    else if ((x == zero) && (y == one)) "i"
    else if ((x == zero) && (y == -one)) "-i"
    else if (y == zero) x.toString
    else if (x == zero) y.toString + "*i"
    else {
      val a = x.toString
      val b = y.abs.toString
      a + " " + (if (y < zero) "-" else "+") + " " + (if (b == "1") "i" else  b + "*i")
    }
  }
}

object Complex {
  import scala.language.implicitConversions
  val zero = BigDecimal(0)
  val one = BigDecimal(1)
  implicit def toComplex(a: BigDecimal) = Complex(a, zero)
}