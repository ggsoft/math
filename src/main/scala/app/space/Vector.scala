package app.space

import app.matrix._
import app.number.{Number => Num}

class Vector[A](override val c: A*)(implicit f: A => Num[A]) extends Point[A](c:_*) {
  def **(v: Vector[A])(implicit f: A => Num[A]) = (c zip v.c).map(x => x._1 * x._2).reduceLeft(_ + _) // scalar product
  def ||(v: Vector[A])(implicit f: A => Num[A]) = (c zip v.c).forall(x => x._1 == x._2) // is parallel
  def |-(v: Vector[A])(implicit f: A => Num[A]) =  (this ** v) == 0 // is orthogonal
  val mod: A = (this ** this).sqrt // length
  val cos: Option[Point[A]] = if (mod > c(0).zero) Some(this/mod) else None // directional cosines
  def cos(v: Vector[A])(implicit f: A => Num[A]) = (this ** v)/(this.mod*v.mod) // cosines of angle with another vector
  def x(s: Vector[A]*)(implicit f: A => Num[A]): Vector[A] = { // generalization of vector product
    if (n < 3) throw new Exception("Dimension should be > 2")
    if (s.size != n-2) throw new Exception(s"Seq of ${n-2} vectors expected")
    val a = Seq(this.c,this.c) ++ s.map(_.c)
    val qm = QMatrix(a)
    val one = a(0)(0).one
    val c = a.head.indices.map(j => qm.sign(j)*det(qm.minor(0,j)))
    Vector(c:_*)
  }
}

object Vector {
  def apply[A](c: A*)(implicit f: A => Num[A]) = new Vector(c:_*)
  def apply[A](p: Point[A])(implicit f: A => Num[A]) = new Vector(p.c:_*)
  def apply[A](p1: Point[A], p2: Point[A])(implicit f: A => Num[A]) = new Vector((p2 - p1).c:_*)
}
