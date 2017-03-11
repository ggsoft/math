package app

import app.matrix._
import app.number._
import app.space._
import app.linear._

object Main {

  def main(args: Array[String]): Unit = {
    val div = "-"*93
    println(div)
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
    println(div)
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
    println(div)
    val c1 = Complex(BigDecimal(2),BigDecimal(-3))
    val c2 = Complex(BigDecimal(1),BigDecimal(1))
    println("(2-3i)*(1+i) = "+c1*c2)
    println("(2-3i)^50 = "+(c1.pow(50)))
    println("(1+i)^100 = "+(c2.pow(100)))
    println("-"*93)
    val f = Frac(BigInt("100250657902956342367209564"), BigInt("152232480519304075446503412"))
    println("Fraction: "+f+" = "+f.reduce)
    println(div)
    val a: Seq[Seq[Double]] = Seq(Seq(4,3,-4,2,2), Seq(2,-1,-3,-4,2), Seq(3,1,1,2,-1), Seq(1,2,3,4,-1), Seq(-1,1,-1,-2,3))
    val qm = QMatrix(a)
    println(qm+"\n")
    println("Determinant = "+det(qm))
    println(div)
    val v2 = Vector(6.0,7,10)
    val v3 = Vector(8.0,5,9)
    val v4 = Vector(6.0,7,10,5)
    val v5 = Vector(8.0,5,9,7)
    val v6 = Vector(1.0,1,1,1)
    println("Generalization of vector product:")
    println(v2 + " x " + v3 + " = "+(v2 x v3))
    println(v3 + " x "+ v2 + " = "+(v3 x v2))
    println
    println("v1 = "+v4)
    println("v2 = "+v5)
    println("v3 = "+v6)
    println("v1 x (v2,v3) = "+(v4 x (v5,v6)))
    println(div)
    val v7 = Vector(2.0,3,5)
    val v8 = Vector(1.0,4,4)
    val v9 = Vector(3.0,5,7)
    println("v1 = "+v7)
    println("v2 = "+v8)
    println("v3 = "+v9)
    println("Mixed product = "+mix(v7,v8,v9))
    println("Volume of parallelepiped = "+volume(v7,v8,v9))
    println(div)
    val b: Seq[Seq[BigDecimal]] = Seq(Seq(1,-2),Seq(3,-4))
    val qmb = QMatrix(b)
    println(qmb+"\n")
    println("Power 100 = \n"+qmb.pow(100))
    println("Inverse matrix = \n"+qmb.inverse)
    println(div)
    val c: Seq[Seq[BigDecimal]] = Seq(Seq(1, 1),Seq(0, 1))
    val qmc = QMatrix(c)
    println(qmc+"\n")
    println("Power 100 = \n"+qmc.pow(100))
    println("Inverse matrix = \n"+qmc.inverse)
    println(div)
    println("System of linear equations:")
    println("2x + 3y - z = 9")
    println("x - 2y + z = 3")
    println("x + 2z = 2")
    val aa: Seq[Seq[BigDecimal]] =  Seq(Seq(2,3,-1),Seq(1,-2,1),Seq(1,0,2))
    val mm = QMatrix(aa)
    val vv = Vector[BigDecimal](9,3,2)
    println("Solution: "+System(mm,vv).cramer)
    println(div)
  }
}
