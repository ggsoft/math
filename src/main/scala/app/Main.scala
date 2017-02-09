package app

import app.number._
import app.space._

object Main {

  def main(args: Array[String]): Unit = {
    println("-"*93)
    val p = Point(1.0,0.0,0.0)
    val v = Vector(1.0,1.0,1.0)
    val  plane = Plane(p,v)
    println("Plane: " + plane)
    List(
      Point(0.0,0.0,0.0),
      Point(1.0,0.0,0.0),
      Point(0.0,1.0,0.0),
      Point(0.0,0.0,1.0),
      Point(1.0,1.0,1.0)
    ).foreach(x =>
      println("Distance from "+x+" = "+ plane.dist(x))
    )
    println("-"*93)
    val p1 = Point(BigDecimal(3),BigDecimal(0),BigDecimal(0))
    val v1 = Vector(BigDecimal(2),BigDecimal(4),BigDecimal(-4))
    val  plane1 = Plane(p1,v1)
    println("Plane: " + plane1)
    List(
      Point(BigDecimal(0),BigDecimal(0),BigDecimal(0)),
      Point(BigDecimal(0),BigDecimal(3),BigDecimal(6)),
      Point(BigDecimal("1.212112121212232323232323333232323"),BigDecimal(3),BigDecimal(5))
    ).foreach(x =>
      println("Distance from "+x+" = "+ plane1.dist(x))
    )
    println("-"*93)
    val c1 = Complex(BigDecimal(2),BigDecimal(-3))
    val c2 = Complex(BigDecimal(1),BigDecimal(1))
    println("(2-3i)*(1+i) = "+c1*c2)
    println("(2-3i)^50 = "+(c1^50))
    println("(1+i)^100 = "+(c2^100))
    println("-"*93)
    val f = Frac(BigInt("100250657902956342367209564"), BigInt("152232480519304075446503412"))
    println(f+" = "+f.reduce)
    println("-"*93)
  }
}
