package app.number

class Frac(val a: BigInt, val b: BigInt) {
  def reduce = {
    val d = a gcd b
    if (d > 0) Frac(a/d, b/d) else this
  }
  def +(f: Frac) = Frac(a*f.b+b*f.a, b*f.b).reduce
  def -(f: Frac) = Frac(a*f.b-b*f.a, b*f.b).reduce
  def *(f: Frac) = Frac(a*f.a, b*f.b).reduce
  def /(f: Frac) = Frac(a*f.b, b*f.a).reduce
  def pow(n: Int) = Frac(a.pow(n), b.pow(n)).reduce
  def ==(f: Frac) = a*f.b == b*f.a
  def >(f: Frac) = a*f.b > b*f.a
  def >=(f: Frac) = a*f.b >= b*f.a
  def <(f: Frac) = a*f.b < b*f.a
  def <=(f: Frac) = a*f.b <= b*f.a

  def toDouble = a.toDouble/b.toDouble
  def toBigDecimal = BigDecimal(a)/BigDecimal(b)

  override def equals(obj: Any): Boolean = {
    if (obj.isInstanceOf[Frac]) {
      val f = obj.asInstanceOf[Frac]
      a*f.b == b*f.a
    } else false
  }

  override def hashCode: Int = {
    val r = this.reduce
    13*r.a.hashCode + 19*r.b.hashCode
  }

  override def toString = {
    if (a == b) "1"
    else if (b == 1) s"$a"
    else s"$a/$b"
  }
}

object Frac {
  def apply(a: BigInt, b: BigInt) =  new Frac(a,b)
  def unapply(f: Frac) = Some((f.a,f.b))
  val zero = Frac(0,1)
  val one = Frac(1,1)
  import scala.language.implicitConversions
  implicit def fromBigInt(a: BigInt) = Frac(a,1)
  implicit def fromInt(a: Int) = Frac(a,1)
  implicit def fromLong(a: Long) = Frac(a,1)
}
