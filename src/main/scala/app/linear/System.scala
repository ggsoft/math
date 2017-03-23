package app.linear

import app.matrix._
import app.space.{Plane, Vector}
import app.number.{Number => Num}

/*
Solving systems of linear equations

ax + by = c
dx + ey = f

m = QMatrix(Seq(Seq(a,b),Seq(d,e)))
v = Vector(c,f)
*/

class System[A <% Num[A]](val m: QMatrix[A], val v: Vector[A]) {
  if (m.n != v.n) throw new Exception("Data mismatch")
  lazy val md = det(m) // main determinant
  val zero = v.c(0).zero

  def cramer: Vector[A] = { // Cramer’s rule
    def ins(k: Int): QMatrix[A] = {
      val a = m.tr.a
      QMatrix(Matrix(a.indices.map(j => if (j == k) v.c else a(j))).tr.a)
    }

    val dd = (0 to m.n - 1).map(k => det(ins(k)))

    val allZero = dd.forall(_ == zero) && (md == zero)
    if (allZero) throw new Exception("The system is undefined (infinite set of solutions)")
    else if (md == zero) throw new Exception("The system has no solutions")
    else Vector(dd.map(_/md):_*)
  }

  def matrix: Vector[A] = {  // Inverse matrix solution
    Vector((m.inverse ** Matrix(Seq(v.c)).tr).tr.a(0):_*)
  }

  override def toString = (m.a zip v.c).map(p => Plane.left(Vector(p._1:_*)) + " = " + p._2).mkString("\n")
}

object System {
  def apply[A <% Num[A]](m: QMatrix[A], v: Vector[A]) = new System(m, v)
}
