package app.space

import app.number.{Number => Num}

class Vector[A](override val c: A*)(implicit f: A => Num[A]) extends Point[A](c:_*) {
  def **(v: Vector[A])(implicit f: A => Num[A]) = (c zip v.c).map(x => x._1 * x._2).reduceLeft(_ + _) // scalar product
  def ||(v: Vector[A])(implicit f: A => Num[A]) = (c zip v.c).forall(x => x._1 == x._2) // is parallel
  def |-(v: Vector[A])(implicit f: A => Num[A]) =  (this ** v) == 0 // is orthogonal
  val mod: A = (this ** this).sqrt // length
  val cos: Option[Point[A]] = if (mod > c(0).zero) Some(this/mod) else None // directional cosines
  def cos(v: Vector[A])(implicit f: A => Num[A]) = (this ** v)/(this.mod*v.mod) // cosines of angle with another vector
}

object Vector {
  def apply[A](c: A*)(implicit f: A => Num[A]) = new Vector(c:_*)
  def apply[A](p: Point[A])(implicit f: A => Num[A]) = new Vector(p.c:_*)
  def apply[A](p1: Point[A], p2: Point[A])(implicit f: A => Num[A]) = new Vector((p2 - p1).c:_*)
}
