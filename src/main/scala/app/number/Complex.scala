package app.number

import app.number.{Number => Num}

case class Complex[A <% Num[A]](x: A, y: A) {
  private val d = x*x + y*y
  val mod = d.sqrt
  def unary_~ = Complex(x, -y)
  def unary_- = Complex(-x, -y)
  def unary_+ = this
  def inv = ~Complex(x/d, y/d) // inverse number
  def +(z: Complex[A]) = Complex(x + z.x, y + z.y)
  def -(z: Complex[A]) = Complex(x - z.x, y - z.y)
  def *(z: Complex[A]) = Complex(x*z.x - y*z.y, x*z.y + y*z.x)
  def /(z: Complex[A]) = this * z.inv
  def pow(n: Int): Complex[A] = { // power
    if (n > 0) this * (pow(n-1))
    else if (n < 0) Complex(x.one, x.zero) / (pow(-n))
    else Complex(x.one, x.zero)
  }
  override def toString = {
    if ((x == x.zero) && (y == y.zero)) "0"
    else if ((x == x.zero) && (y == y.one)) "i"
    else if ((x == x.zero) && (y == -y.one)) "-i"
    else if (y == y.zero) x.toString
    else if (x == x.zero) y.toString + "*i"
    else {
      val a = x.toString
      val b = y.abs.toString
      a + " " + (if (y < y.zero) "-" else "+") + " " + (if (b == "1") "i" else  b + "*i")
    }
  }
}

object Complex {
  import scala.language.implicitConversions
  implicit def toComplex[A <% Num[A]](a: A) = Complex(a, a.zero)
}