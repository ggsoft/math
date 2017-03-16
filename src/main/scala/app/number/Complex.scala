package app.number

case class Complex(a: BigDecimal, b: BigDecimal) {
  private val d = a*a + b*b
  val mod = app.number.sqrt(d)
  val zero = Complex.zero
  val one = Complex.one
  def unary_~ = Complex(a, -b)
  def unary_- = Complex(-a, -b)
  def unary_+ = this
  def inv = ~Complex(a/d, b/d) // inverse number
  def +(z: Complex) = Complex(a + z.a, b + z.b)
  def -(z: Complex) = Complex(a - z.a, b - z.b)
  def *(z: Complex) = Complex(a*z.a - b*z.b, a*z.b + b*z.a)
  def /(z: Complex) = this * z.inv

  def sqrt = {
    import app.number.{sqrt => qr}
    val x = qr((mod + a) / 2)
    val y = b / (x * 2)
    Complex(x, y)
  }

  def pow(n: Int): Complex = { // power
    if (n > 0) this * (pow(n-1))
    else if (n < 0) Complex(one, zero) / (pow(-n))
    else Complex(one, zero)
  }

  override def toString = {
    if ((a == zero) && (b == zero)) "0"
    else if ((a == zero) && (b == one)) "i"
    else if ((a == zero) && (b == -one)) "-i"
    else if (b == zero) a.toString
    else if (a == zero) b.toString + "*i"
    else {
      val x = a.toString
      val y = b.abs.toString
      x + " " + (if (b < zero) "-" else "+") + " " + (if (y == "1") "i" else  y + "*i")
    }
  }
}

object Complex {
  import scala.language.implicitConversions
  val zero = BigDecimal(0)
  val one = BigDecimal(1)
  implicit def toComplex(a: BigDecimal) = Complex(a, zero)
}